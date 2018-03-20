function subscribeToOutput(outputContainer) {
  var preElement = outputContainer.children.first;
  var jobId = outputContainer.dataset.jobId;

  console.info('Starting streaming of deploy output for job:', jobId);

  if (jobId) {
    var stream = new EventSource('/todo');
    stream.onmessage = function (e) {
      //var line = document.createElement('div');
    };
  }
};

(function () {
  var commandOutput = document.querySelector('.command-output');
  if (commandOutput) {
    subscribeToOutput(commandOutput);
  }
})();
