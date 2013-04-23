package com.timgroup.matchless

import org.specs2.matcher.MustMatchers._
import com.timgroup.matchless.Matchers._

object MatcherMatcherWorksheet {
  (contain("foo") must matchTheValue("football")).message
                                                  //> res0: String = matched with the message ''football' contains 'foo''

  (contain("bar") must failToMatchTheValue("battalion")).message
                                                  //> res1: String = failed to match, with the message ''battalion' doesn't contai
                                                  //| n 'bar''
                                                  
  (contain("baz") must matchTheValue("bays")).message
                                                  //> res2: String = failed to match with the message ''bays' doesn't contain 'baz
                                                  //| ''
                                                  
  (contain("xyzzy") must failToMatchTheValue("abracadabra").withTheMessage("'abracadabra' doesn't contain 'xyzzy'")).message
                                                  //> res3: String = failed to match, with the message ''abracadabra' doesn't cont
                                                  //| ain 'xyzzy''

  (contain("xyzzy") must failToMatchTheValue("abracadabra").withTheMessage("you idiot!")).message
                                                  //> res4: String = failed to match, but ''abracadabra' doesn't contain 'xyzzy'' 
                                                  //| is not equal to 'you idiot!'
                                                  
  (contain("xyzzy") must matchTheValue("xyzzy!").withTheMessage("fantastic!")).message
                                                  //> res5: String = matched but ''xyzzy!' contains 'xyzzy'' is not equal to 'fant
                                                  //| astic!'
                                                  
  (contain("xyzzy") must matchTheValue("xyzzy!").withMessageLike(contain("fantastic!"))).message
                                                  //> res6: String = matched but ''xyzzy!' contains 'xyzzy'' doesn't contain 'fant
                                                  //| astic!'

  (contain("xyzzy") must matchTheValue("xyzzy!").withMessageLike(contain("contains"))).message
                                                  //> res7: String = matched with the message ''xyzzy!' contains 'xyzzy''
}
