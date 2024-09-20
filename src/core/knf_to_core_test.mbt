test "knf_to_core" {
  let src =
    #|fn matmul(l: Int, m: Int, n: Int) -> Unit {
    #|  fn loop1(i: Int) -> Unit {
    #|    if 0 <= i {
    #|      fn loop2(j: Int) -> Unit {
    #|        if 0 <= j {
    #|          fn loop3(k: Int) -> Unit {
    #|            if 0 <= k {
    #|              let _ = i + j + k;
    #|              loop3(k - 1)
    #|            } else {
    #|              ()
    #|            }
    #|          };
    #|          let _ = loop3(m - 1);
    #|          loop2(j - 1)
    #|        } else {
    #|          ()
    #|        }
    #|      };
    #|      let _ = loop2(n - 1);
    #|      loop1(i - 1)
    #|    } else {
    #|      ()
    #|    }
    #|  };
    #|  loop1(l - 1)
    #|};
  let parser = @parser.Parser::new(src)
  let ast = parser.parse!().inner
  let typer = @typer.Typer::new(ast)
  typer.check_all!()
  let knf_env = @knf.KnfEnv::new(@typer.externals())
  let preprocessed = knf_env.syntax_preprocess(ast, false, false)
  let knf = knf_env.to_knf!(preprocessed)
  let core_env = @core.CoreEnv::new()
  let core = core_env.build!(knf)
  let _ = core

}
