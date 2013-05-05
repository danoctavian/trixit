#!/usr/bin/env node

var util = require('util'),
    http = require('http'),
    fs = require('fs'),
    url = require('url'),
    events = require('events'),
    socketio = require("socket.io"),
    express = require("express"),
    passport = require('passport'),
    LocalStrategy = require('passport-local').Strategy,
    mocks = require("./mocks");


var DEFAULT_PORT = 8000
var SESSION_STORE_SECRET = 'my_a$$'
var User = new mocks.MockCredentialsStore(null)

console.log("running express server")

// auth stuff
function checkAuth(req, res, next) {
  if (!req.session.user_id) {
    res.send('You are not authorized to view this page')
  } else {
    next()
  }
}



function main(argv) {
  var app = express()
  var server = http.createServer(app)
  var d = __dirname
  d = d.substring(0, d.lastIndexOf("/") + 1)


  function checkAuth(req, res, next){
    console.log("checking auth...")
    console.log("request is auth " + req.isAuthenticated())
    next()
  }

  /* middleware */
  app.configure(function () {
    app.use(express.cookieParser())
    app.use(express.bodyParser())
    app.use(express.static(d))

    app.use(express.session({ secret: SESSION_STORE_SECRET}))
    app.use(checkAuth)
    app.use(passport.initialize())
    app.use(passport.session())
    app.use(app.router)
    
  })

  
  app.post('/login', function (req, res) {
    var post = req.body;
    if (post.user == 'john' && post.password == 'johnspassword') {
      req.session.user_id = johns_user_id_here
      res.redirect('/my_secret_page')
    } else {
      res.send('Bad user/pass')
    }
  })


/*
  passport.use(new LocalStrategy(
    function(username, password, done) {
      console.log("local strategy function called with " + username + " " + password)
      User.findOne({ username: username }, function (err, user) {
        if (err) { return done(err) }
        if (!user) {
          return done(null, false, { message: 'Incorrect username.' })
        }
        if (!user.validPassword(password)) {
          return done(null, false, { message: 'Incorrect password.' })
        }
        return done(null, user)
      })
    }
  )) 
*/


  /*
  passport.serializeUser(function(user, done) {
    console.log("calling serialize user")
    done(null, user.id)
  })
  passport.deserializeUser(function(obj, done) {
    console.log("%%%%%%% deserialized user " + util.inspect(obj))
    done(null, obj)
  })

  
  app.get('/bawlshit', function (req, res) {
    console.log("responding to bawlshit")
    res.json("bullshit")
  })



  app.post("/login",  passport.authenticate('local', {session: true}), function (req, res) {
    console.log("login post contains " + util.inspect(req.body))
    console.log("request is authenticated my ass " + req.isAuthenticated())
    //console.log("login request contains " + req.body.username +  " " + req.body.password)
  })
  */

  server = app.listen(DEFAULT_PORT)

  var io = socketio.listen(server)
  io.set('log level', 1)
  io.sockets.on('connection', function (socket) {
    socket.emit('gameInit', { stage: "waitingForOthers", players: [{name: "shag"}, {name: "your mom"}]})

    socket.on("auth", function(data) {
      console.log("received authentification data")
    })
    socket.on('gameAction', function (data) {
      console.log("received game action: " + data)
    })
  })
}

// Must be last,
main(process.argv);
