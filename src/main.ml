open QCheck

module APIConf =
struct
    type sut
                
    type state

    type cmd
    
    let arb_cmd state

    let init_state
    let next_state cmd state

    let init_sut () 
    let cleanup _  

    let run_cmd cmd state actual

    let precond _ _ = true
end
module APItest = QCSTM.Make(APIConf)
;;
QCheck_runner.run_tests ~verbose:true
  [APItest.agree_test ~count:100 ~name:"Api Model agreement"]
