package com.olvind.crud.server

import com.typesafe.slick.testkit.util.{StandardTestDBs, DriverTest, Testkit}
import org.junit.runner.RunWith

@RunWith(classOf[Testkit])
class H2MemTest extends DriverTest(StandardTestDBs.H2Mem)
