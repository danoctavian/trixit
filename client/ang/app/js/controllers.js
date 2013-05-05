'use strict';

/* Controllers */


function GameCtrl($scope, $timeout, Phone) {
  var socket = null
  /* utils*/
  var getCardUrl = function(index) {return "img/cards/" + index + ".jpg"}
  
  function sendMessage(msg) {
    console.log("sending message " + msg)
    //socket.emit("gameAction", msg)
  }

  
  /*
  socket.on("gameInit", function (data) {
    console.log("initialized game with " + data)
  })
  */
  
  /* constants, enums and types */
  function Card(index) {
    this.index = index
    this.imageUrl = getCardUrl(index)
  }

  var stageDescs = {voteCard: {name: "Vote the card!", instructions: ""},
                    chooseCard: {name: "Choose a card!", instructions: ""},
                    waitingForOthers: {name: "Waiting for the other players...", instructions: ""},
                    sayPhrase: {name: "Say the word!", instructions: ""}}
  var stage = "waitingForOthers"
  function setStage(stageType) {
    $scope.stageDesc = stageDescs[stageType]
    stage = stageType
  }

  setStage("waitingForOthers")

  $scope.players = [{name: "fucker", score: -1}, {name: "mother", score: 9000}]
  $scope.phones = Phone.query()
  $scope.errorField = ""

  //console.log("this is the socket" + socket)

  $scope.phrase = "shit son"
  $scope.clickedPhone = function(name) {console.log("clicked on phone " + name)}
  $scope.pressed = function() {console.log("hello")}

  // cards
  $scope.ownCards = [1, 2].map(function(i) {return new Card(i)})
  $scope.tableCards = [3, 4, 5, 6].map(function(i) {return new Card(i)})

  function setErrorMessage(msg) {
    $scope.errorField = msg
    $timeout(function(){$scope.errorField = ""; console.log("timeout called")}, 3000)
  }

  //fields
  $scope.phraseField = ""
  //client events
  $scope.clickedOwnCard = function(card) {
    console.log("clicked own card:" + card)
    if (stage === "chooseCard") {
      sendMessage({type: "ownCardChoice", value: card.index})
    } else {
      setErrorMessage("no card selection required")
    }
  }

  $scope.clickedTableCard = function(card) {
    console.log("clicked table card:" + card) 
    if (stage === "voteCard") {
      sendMessage({type: "cardVote", value: card.index})
    } else {
      setErrorMessage("no voting required")
    }
  }

  $scope.wrotePhrase = function() {
    console.log("wrote phrase" + phrase)       
    if (stage === "sayPhrase") {
      sendMessage({type: "phrase", value: $scope.phraseField})
    } else {
      setErrorMessage("no phrase required")
    }
  }
}

function LoginCtrl($scope, $http) {
  $scope.name = "faggot"
  $scope.username = ""
  $scope.password = ""
  $scope.submitLogin = function() {
    console.log("the username and password are " + $scope.username + " " + $scope.password)
    $http.post("/login", {username: $scope.username, password: $scope.password}).success(function (data) {
      console.log("got response from post " + data)
    })
  }
  $scope.testField = "hmm this is just a test field"

  $http.get('/bawlshit', null).success(function (data) {
        console.log("Rest call answer " + data)
  })


}

function PhoneListCtrl($scope, Phone) {
  $scope.phones = Phone.query();
  $scope.orderProp = 'age';
}

function PhoneDetailCtrl($scope, $routeParams, Phone) {
  $scope.phone = Phone.get({phoneId: $routeParams.phoneId}, function(phone) {
    $scope.mainImageUrl = phone.images[0];
  });

  $scope.setImage = function(imageUrl) {
    $scope.mainImageUrl = imageUrl;
  }
}

//PhoneDetailCtrl.$inject = ['$scope', '$routeParams', 'Phone'];
