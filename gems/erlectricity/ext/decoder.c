#include "ruby.h"
#include <stdlib.h>
#include <string.h>

// constants
static int VERSION = 131;

static int SMALL_INT = 97;
static int INT = 98;

static int SMALL_BIGNUM = 110;
static int LARGE_BIGNUM = 111;

static int FLOAT = 99;

static int ATOM = 100;
static int REF = 101;           // old style reference
static int NEW_REF = 114;
static int PORT = 102;          // not supported accross node boundaries
static int PID = 103;

static int SMALL_TUPLE = 104;
static int LARGE_TUPLE = 105;

static int NIL = 106;
static int STRING = 107;
static int LIST = 108;
static int BIN = 109;

static int FUN = 117;
static int NEW_FUN = 112;

static VALUE mErlectricity;
static VALUE cDecoder;
void Init_decoder();

VALUE method_read_any_from(VALUE klass, VALUE rString);

VALUE read_any_raw(unsigned char **pData);

// checkers

void check_int(int num) {
  char buf[17];
  sprintf(buf, "%u", num);
  rb_raise(rb_eStandardError, buf);
}

void check_str(char *str) {
  rb_raise(rb_eStandardError, str);
}

// string peekers/readers

unsigned int peek_1(unsigned char **pData) {
  return (unsigned int) **pData;
}

unsigned int peek_2(unsigned char **pData) {
  return (unsigned int) ((**pData << 8) + *(*pData + 1));
}

unsigned int peek_4(unsigned char **pData) {
  return (unsigned int) ((**pData << 24) + (*(*pData + 1) << 16) + (*(*pData + 2) << 8) + *(*pData + 3));
}

unsigned int read_1(unsigned char **pData) {
  unsigned int val = peek_1(pData);
  *pData += 1;
  return val;
}

unsigned int read_2(unsigned char **pData) {
  unsigned int val = peek_2(pData);
  *pData += 2;
  return val;
}

unsigned int read_4(unsigned char **pData) {
  unsigned int val = peek_4(pData);
  *pData += 4;
  return val;
}

// tuples, lists

VALUE read_small_tuple(unsigned char **pData) {
  if(read_1(pData) != SMALL_TUPLE) {
    rb_raise(rb_eStandardError, "Invalid Type, not a small tuple");
  }
  
  int arity = read_1(pData);
  
  VALUE array = rb_ary_new2(arity);
  
  int i;
  for(i = 0; i < arity; ++i) {
    rb_ary_store(array, i, read_any_raw(pData));
  }
  
  return array;
}

VALUE read_large_tuple(unsigned char **pData) {
  if(read_1(pData) != LARGE_TUPLE) {
    rb_raise(rb_eStandardError, "Invalid Type, not a large tuple");
  }
  
  int arity = read_4(pData);
  
  VALUE array = rb_ary_new2(arity);
  
  int i;
  for(i = 0; i < arity; ++i) {
    rb_ary_store(array, i, read_any_raw(pData));
  }
  
  return array;
}

VALUE read_list(unsigned char **pData) {
  if(read_1(pData) != LIST) {
    rb_raise(rb_eStandardError, "Invalid Type, not an erlang list");
  }
  
  int size = read_4(pData);
  
  VALUE array = rb_ary_new2(size);
  
  int i;
  for(i = 0; i < size; ++i) {
    rb_ary_store(array, i, read_any_raw(pData));
  }
  
  return array;
}

// primitives

void read_string_raw(unsigned char *dest, unsigned char **pData, int length) {
  strncpy((char *) dest, (char *) *pData, length);
  *(dest + length) = (unsigned char) 0;
  *pData += length;
}

VALUE read_bin(unsigned char **pData) {
  // fail("Invalid Type, not an erlang binary") unless read_1 == BIN
  if(read_1(pData) != BIN) {
    rb_raise(rb_eStandardError, "Invalid Type, not an erlang binary");
  }
  
  int length = read_4(pData);
  
  unsigned char buf[length + 1];
  read_string_raw(buf, pData, length);
  
  return rb_str_new2((char *) buf);
}

VALUE read_string(unsigned char **pData) {
  if(read_1(pData) != STRING) {
    rb_raise(rb_eStandardError, "Invalid Type, not an erlang string");
  }
  
  int length = read_2(pData);
  
  unsigned char buf[length + 1];
  read_string_raw(buf, pData, length);
  
  VALUE array = rb_ary_new2(length);
  
  int i = 0;
  for(i; i < length; ++i) {
    rb_ary_store(array, i, INT2NUM(*(buf + i)));
  }
  
  return array;
}

VALUE read_atom(unsigned char **pData) {
  if(read_1(pData) != 100) {
    rb_raise(rb_eStandardError, "Invalid Type, not an atom");
  }
  
  int length = read_2(pData);
  
  unsigned char buf[length + 1];
  read_string_raw(buf, pData, length);
  
  return ID2SYM(rb_intern((char *) buf));
}

VALUE read_small_int(unsigned char **pData) {
  if(read_1(pData) != 97) {
    rb_raise(rb_eStandardError, "Invalid Type, not a small int");
  }
  
  int value = read_1(pData);
  
  return INT2FIX(value);
}

VALUE read_int(unsigned char **pData) {
  if(read_1(pData) != 98) {
    rb_raise(rb_eStandardError, "Invalid Type, not an int");
  }
  
  long long value = read_4(pData);
  
  long long negative = ((value >> 31) & 0x1 == 1);
  
  if(negative) {
    value = (value - ((long long) 1 << 32));
  }
  
  return INT2FIX(value);
}

VALUE read_small_bignum(unsigned char **pData) {
  if(read_1(pData) != SMALL_BIGNUM) {
    rb_raise(rb_eStandardError, "Invalid Type, not a small bignum");
  }
  
  unsigned int size = read_1(pData);
  unsigned int sign = read_1(pData);
  
  VALUE num = INT2NUM(0);
  VALUE tmp;
  
  unsigned char buf[size + 1];
  read_string_raw(buf, pData, size);
  
  int i;
  for(i = 0; i < size; ++i) {
    tmp = INT2FIX(*(buf + i));
    tmp = rb_funcall(tmp, rb_intern("<<"), 1, INT2NUM(i * 8));
    
    num = rb_funcall(num, rb_intern("+"), 1, tmp);
  }
  
  if(sign) {
    num = rb_funcall(num, rb_intern("*"), 1, INT2NUM(-1));
  }
  
  return num;
}

VALUE read_large_bignum(unsigned char **pData) {
  if(read_1(pData) != LARGE_BIGNUM) {
    rb_raise(rb_eStandardError, "Invalid Type, not a small bignum");
  }
  
  unsigned int size = read_4(pData);
  unsigned int sign = read_1(pData);
  
  VALUE num = INT2NUM(0);
  VALUE tmp;
  
  unsigned char buf[size + 1];
  read_string_raw(buf, pData, size);
  
  int i;
  for(i = 0; i < size; ++i) {
    tmp = INT2FIX(*(buf + i));
    tmp = rb_funcall(tmp, rb_intern("<<"), 1, INT2NUM(i * 8));
    
    num = rb_funcall(num, rb_intern("+"), 1, tmp);
  }
  
  if(sign) {
    num = rb_funcall(num, rb_intern("*"), 1, INT2NUM(-1));
  }
  
  return num;
}

VALUE read_nil(unsigned char **pData) {
  if(read_1(pData) != NIL) {
    rb_raise(rb_eStandardError, "Invalid Type, not a nil list");
  }
  
  return rb_ary_new2(0);
}

VALUE read_any_raw(unsigned char **pData) {
  switch(peek_1(pData)) {
    case 97:
      return read_small_int(pData);
      break;
    case 98:
      return read_int(pData);
      break;
    case 100:
      return read_atom(pData);
      break;
    case 104:
      return read_small_tuple(pData);
      break;
    case 105:
      return read_large_tuple(pData);
      break;
    case 106:
      return read_nil(pData);
      break;
    case 107:
      return read_string(pData);
      break;
    case 108:
      return read_list(pData);
      break;
    case 109:
      return read_bin(pData);
      break;
    case 110:
      return read_small_bignum(pData);
      break;
    case 111:
      return read_large_bignum(pData);
      break;
  }
  return Qnil;
}

VALUE method_read_any_from(VALUE klass, VALUE rString) {
  unsigned char *data = (unsigned char *) StringValuePtr(rString);
  
  unsigned char **pData = &data;
  
  // check protocol version
  if(read_1(pData) != VERSION) {
    rb_raise(rb_eStandardError, "Bad Magic");
  }
  
  return read_any_raw(pData);
}

void Init_decoder() {
  mErlectricity = rb_const_get(rb_cObject, rb_intern("Erlectricity"));
  cDecoder = rb_define_class_under(mErlectricity, "Decoder", rb_cObject);
  rb_define_singleton_method(cDecoder, "read_any_from", method_read_any_from, 1);
}