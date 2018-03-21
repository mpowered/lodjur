function subscribeToOutput(outputContainer) {
  var preElement = outputContainer.firstElementChild;
  var jobId = outputContainer.dataset.jobId;
  var lastLineAt = outputContainer.dataset.lastLineAt ? new Date(outputContainer.dataset.lastLineAt) : 0;

  if (jobId) {
    console.info('Starting streaming of deploy output for job', jobId, 'as of', lastLineAt.toString());
    outputContainer.classList.add('streaming');

    var stream = new EventSource('/jobs/' + jobId + '/output?from=' + lastLineAt.toString());

    stream.addEventListener('output', function (e) {
      var event = JSON.parse(e.data);
      console.log('Got stuff sent at', event.outputEventTime);

      event.outputEventLines.forEach(function (outputLine) {
        var line = document.createElement('div');
        line.classList.add('line');
        line.append(outputLine);
        preElement.appendChild(line);
      });
    });

    stream.addEventListener('end', function (e) {
      console.log('Output stream ended.');
      outputContainer.classList.remove('streaming');
      stream.close();
    });

    stream.onerror = function (e) {
      outputContainer.classList.remove('streaming');
      console.error(e);
      stream.close();
    };
  }
};

(function () {
  var commandOutput = document.querySelector('.command-output');
  if (commandOutput) {
    subscribeToOutput(commandOutput);
  }
})();
