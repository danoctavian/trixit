var mocks = require("./mocks.js")
var util = require("util")


console.log("fucking around")

var userMock = new mocks.MockCredentialsStore()


util.inspect(userMock.findOne({username: "dan"}, function(err, user) {
	console.log("err: " + util.inspect(err) + " user" + util.inspect(user.validPassword("themotherfucker")))  
}))


