# Selenium-based functional tests #

Nurpawiki contains a series of functional test cases that tests various simple interactions.  The tests are executed using [Selenium IDE](http://www.openqa.org/selenium-ide/).

All tests must pass for each new release.

## Running tests ##

### Preparations ###
  * Make sure you have the Selenium IDE installed in Firefox.

### Running the tests ###
Note that you may want to modify localhost to be some other host.  If so, you likely need to modify the file path of the test cases as well.

  * Make sure you have Nurpawiki running on `localhost` at port 8080.
  * Point your browser to `chrome://selenium-ide/content/selenium/TestRunner.html?baseURL=http://localhost:8080&test=file:///home/<user>/<path_to_nurpawiki>/trunk/test/functional/TestSuite.html`