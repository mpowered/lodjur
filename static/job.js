function subscribeToOutput(outputContainer) {
  try {
    var preElement = outputContainer.children.first;
    var jobId = outputContainer.dataset.jobId;

    console.info('Starting streaming of deploy output for job:', jobId);

    if (jobId) {
      var stream = new EventSource('/jobs/' + jobId + '/output');
      stream.addEventListener('output', function (e) {
        try {
          var event = JSON.parse(e.data);
          console.log('Got stuff sent at', event.outputEventTime, 'with lines:\n', event.outputEventLines);
          //var line = document.createElement('div');
        } catch (e) {
          console.error(e);
        }
      });
    }
  } catch (e) {
    console.error(e);
  }
};

(function () {
  var commandOutput = document.querySelector('.command-output');
  if (commandOutput) {
    subscribeToOutput(commandOutput);
  }
})();
