'use strict';

/* Controllers */

angular.module('myApp.controllers', []).
  controller('MyCtrl1', [function($scope) {
  	$scope.val = 1
  }])
  .controller('MyCtrl2', [function() {

  }]);