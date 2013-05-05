var _ = require("underscore")

function MockCredentialsStore(users) {
	if (!users) {users = [{username: "dan", password: "p"}]} // add a dummy, namely me
	this.users = users
}

MockCredentialsStore.prototype.findOne = function (userData, callback) {
	var result = _.find(this.users, function(u) {return u.username === userData.username})
	if (!result) {
		callback(null, null)
	} else {
		callback(null, {id: 1337, validPassword: function(pass) {return pass === result.password}})
	}
}

module.exports.MockCredentialsStore = MockCredentialsStore