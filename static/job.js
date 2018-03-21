function subscribeToOutput(outputContainer) {
  try {
    var preElement = outputContainer.firstElementChild;
    var jobId = outputContainer.dataset.jobId;

    console.info('Starting streaming of deploy output for job:', jobId);

    if (jobId) {
      var stream = new EventSource('/jobs/' + jobId + '/output');
      stream.addEventListener('output', function (e) {
        try {
          var event = JSON.parse(e.data);
          console.log('Got stuff sent at', event.outputEventTime);

          event.outputEventLines.forEach(function (outputLine) {
            var line = document.createElement('div');
            line.classList.add('line');
            line.append(outputLine);
            preElement.appendChild(line);
          });
        } catch (e) {
          console.error(e);
        }
      });
      stream.onerror = function (e) {
        console.error(e);
        stream.close();
      };
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
