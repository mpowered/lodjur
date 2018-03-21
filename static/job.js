/* Whether `t1` is before `t2`, with a resolution of whole seconds.
 */
function earlierThanInSeconds(t1, t2) {
  console.log(t1, t2);
  return Math.floor(t1.valueOf() / 1000) <  Math.floor(t2.valueOf() / 1000);
}

function renderTimeStamp(t) {
  function padZero(n) {
    return (n < 10) ? '0' + n : '' + n;
  }
  var timestamp = document.createElement('time');
  timestamp.setAttribute('datetime', t.toISOString());
  timestamp.innerText =
    padZero(t.getUTCHours()) + ':' +
    padZero(t.getUTCMinutes()) + ':' +
    padZero(t.getUTCSeconds());
  return timestamp;
}

function subscribeToOutput(outputContainer) {
  var preElement = outputContainer.firstElementChild;
  var jobId = outputContainer.dataset.jobId;
  var lastLineAt = outputContainer.dataset.lastLineAt ? new Date(outputContainer.dataset.lastLineAt) : new Date();

  if (jobId) {
    console.info('Starting streaming of deploy output for job', jobId, 'as of', lastLineAt.toLocaleString());
    outputContainer.classList.add('streaming');

    var stream = new EventSource('/jobs/' + jobId + '/output?from=' + lastLineAt.toISOString());
    var lastTimestamp = lastLineAt;

    stream.addEventListener('output', function (e) {
      var event = JSON.parse(e.data);
      var eventTimestamp = new Date(event.outputEventTime);

      event.outputEventLines.forEach(function (outputLine) {
        var line = document.createElement('div');
        line.classList.add('line');

        // Add timestamp if not same second as before.
        if (true || earlierThanInSeconds(lastTimestamp, eventTimestamp)) {
          line.append(renderTimeStamp(eventTimestamp));
        }
        lastTimestamp = eventTimestamp;

        // Add the output content.
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
      console.error(e.message, e);
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
