require File.dirname(__FILE__) + '/test_helper.rb'

context "When packing to a binary stream" do
  setup do
    @out = StringIO.new('', 'w')
    @encoder = Erlectricity::Encoder.new(@out)
  end
  specify "A symbol should be encoded to an erlang atom" do
    get{@encoder.write_symbol :haha}.should == get_erl("haha")
    write_any(:haha).should == get_erl_with_magic("haha")
  end
  
  specify "A number should be encoded as an erlang number would be" do
    
    #SMALL_INTS
    get{@encoder.write_fixnum 0}.should == get_erl("0")
    get{@encoder.write_fixnum 255}.should == get_erl("255")
    write_any(0).should == get_erl_with_magic("0")
    write_any(255).should == get_erl_with_magic("255")
    
    #INTS
    get{@encoder.write_fixnum 256}.should == get_erl("256")
    get{@encoder.write_fixnum((1 << 27) - 1)}.should == get_erl("#{(1 << 27) - 1}")
    get{@encoder.write_fixnum(-1)}.should == get_erl("-1")
    get{@encoder.write_fixnum(-(1 << 27))}.should == get_erl("#{-(1 << 27)}")
    write_any(256).should == get_erl_with_magic("256")
    write_any((1 << 27) - 1).should == get_erl_with_magic("#{(1 << 27) - 1}")
    write_any(-1).should == get_erl_with_magic("-1")
    write_any(-(1 << 27)).should == get_erl_with_magic("#{-(1 << 27)}")
    
    # #SMALL_BIGNUMS
    # get{@encoder.write_fixnum((1 << word_length))}.should == get_erl("#{(1 << word_length)}")
    # get{@encoder.write_fixnum(-(1 << word_length) - 1)}.should == get_erl("#{-(1 << word_length) - 1}")
    # get{@encoder.write_fixnum((1 << (255 * 8)) - 1)}.should == get_erl("#{(1 << (255 * 8)) - 1}")
    # get{@encoder.write_fixnum(-((1 << (255 * 8)) - 1))}.should == get_erl("#{-((1 << (255 * 8)) - 1)}")
    # 
    # write_any((1 << word_length)).should == get_erl_with_magic("#{(1 << word_length)}")
    # write_any(-(1 << word_length) - 1).should == get_erl_with_magic("#{-(1 << word_length) - 1}")
    # write_any((1 << (255 * 8)) - 1).should == get_erl_with_magic("#{(1 << (255 * 8)) - 1}")
    # write_any(-((1 << (255 * 8)) - 1)).should == get_erl_with_magic("#{-((1 << (255 * 8)) - 1)}")
    # 
    # #LARG_BIGNUMS
    # get{@encoder.write_fixnum((1 << (255 * 8)))}.should == get_erl("#{(1 << (255 * 8))}")
    # get{@encoder.write_fixnum(-(1 << (255 * 8))}.should == get_erl("#{-(1 << (255 * 8)}")
    # get{@encoder.write_fixnum((1 << (512 * 8))}.should == get_erl("#{(1 << (512 * 8))}")
    # get{@encoder.write_fixnum(-((1 << (512 * 8)) - 1))}.should == get_erl("#{-((1 << (512 * 8)) - 1)}")
    # 
    # write_any((1 << (255 * 8))).should == get_erl_with_magic("#{(1 << (255 * 8))}")
    # write_any(-(1 << (255 * 8)).should == get_erl_with_magic("#{-(1 << (255 * 8)}")
    # write_any((1 << (512 * 8))).should == get_erl_with_magic("#{(1 << (512 * 8))}")
    # write_any(-((1 << (512 * 8)) - 1)).should == get_erl_with_magic("#{-((1 << (512 * 8)) - 1)}")
  end
  
  # specify "A float (that is within the truncated precision of ruby compared to erlang) should encode as erlang does" do
  #   get{@encoder.write_float 1.0}.should == get_erl("1.0")
  #   get{@encoder.write_float -1.0}.should == get_erl("-1.0")
  #   get{@encoder.write_float 123.456}.should == get_erl("123.456")
  #   get{@encoder.write_float 123.456789012345}.should == get_erl("123.456789012345")
  # end
  
  specify "An Erlectiricity::NewReference should encode back to its original form" do
    ref_bin = run_erl("term_to_binary(make_ref())")
    ruby_ref = Erlectricity::Decoder.read_any_from(ref_bin)
    
    get{@encoder.write_new_reference(ruby_ref)}.should == ref_bin[1..-1]
    write_any(ruby_ref).should == ref_bin
  end
  
  specify "An Erlectiricity::Pid should encode back to its original form" do
    pid_bin = run_erl("term_to_binary(spawn(fun() -> 3 end))")
    ruby_pid = Erlectricity::Decoder.read_any_from(pid_bin)
    
    get{@encoder.write_pid(ruby_pid)}.should == pid_bin[1..-1]
    write_any(ruby_pid).should == pid_bin
  end
  
  specify "An array written with write_tuple should encode as erlang would a tuple" do
    get{@encoder.write_tuple [1,2,3]}.should == get_erl("{1,2,3}")
    get{@encoder.write_tuple [3] * 255}.should == get_erl("{#{([3] * 255).join(',')}}")
    get{@encoder.write_tuple [3] * 256}.should == get_erl("{#{([3] * 256).join(',')}}")
    get{@encoder.write_tuple [3] * 512}.should == get_erl("{#{([3] * 512).join(',')}}")
  end
  
  specify "An array should by default be written as a tuple" do
    write_any([1,2,3]).should == get_erl_with_magic("{1,2,3}")
    write_any([3] * 255).should == get_erl_with_magic("{#{([3] * 255).join(',')}}")
    write_any([3] * 256).should == get_erl_with_magic("{#{([3] * 256).join(',')}}")
    write_any([3] * 512).should == get_erl_with_magic("{#{([3] * 512).join(',')}}")
  end
  
  specify "An array written with write_list should encode as erlang would a list" do
    get{@encoder.write_list [1,2,300]}.should == get_erl("[1,2,300]")
    get{@encoder.write_list [300] * 255}.should == get_erl("[#{([300] * 255).join(',')}]")
    get{@encoder.write_list [300] * 256}.should == get_erl("[#{([300] * 256).join(',')}]")
    get{@encoder.write_list [300] * 512}.should == get_erl("[#{([300] * 512).join(',')}]")
  end
  
  specify "a string should be encoded as a erlang binary would be" do
    get{@encoder.write_binary "hey who"}.should == get_erl("<< \"hey who\" >>")
    get{@encoder.write_binary ""}.should == get_erl("<< \"\" >>")
    
    write_any("hey who").should == get_erl_with_magic("<< \"hey who\" >>")
    write_any("").should == get_erl_with_magic("<< \"\" >>")
  end
  
  def get
    @encoder.out = StringIO.new('', 'w')
    yield
    @encoder.out.string
  end
  
  def write_any(term)
    @encoder.out = StringIO.new('', 'w')
    @encoder.write_any term
    @encoder.out.string
  end
  
  def get_erl(str)
    get_erl_with_magic(str)[1..-1] #[1..-1] to chop off the magic number
  end
  
  def get_erl_with_magic(str)
    run_erl("term_to_binary(#{str})")
  end
end