function MockCredentialsStore(users) {
	if (!users) {users = [username: "dan", password: "themotherfucker"]} // add a dummy, namely me
	this.users = users
}

MockCredentialsStore.prototype.findOne = function (userData, callback) {

}

module.exports.MockCredentialsStore = MockCredentialsStore