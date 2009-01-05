/**
 * Simple test file used for experimentation at the console.
 */

import org.sublime.amazon.simpleDB.api.Test._
import org.sublime.amazon.simpleDB.Query._
import org.sublime.amazon.simpleDB.Conversions._

val account = loadAccount
val data = account domain "data"

// make sure the domain exists at simpleDB
data create

// define some attributes
val user = attribute("user")
val startDate = attribute("startDate", ISO8610Date)
val visits = attribute("visits", PositiveInt)
val tags = attribute("tags")

// insert some items
data.unique += (user("robin"), startDate(new java.util.Date()), visits(3))
data.unique += (user("jon"), startDate(new java.util.Date()), visits(20))
data.unique += (user("alice"), startDate(new java.util.Date()), visits(15))
data.unique += (user("jack"), startDate(new java.util.Date()), visits(100))

// do some queries
for (i <- data (visits > 16)) { println(user(i).head) }

for (i <- data (visits > 16 and visits < 50)) { println(user(i).head) }

for (i <- data (visits > 1 and visits < 50 sort visits desc)) { println(user(i).head) }

// get rid of the test data
(data items) foreach (_.clear)
