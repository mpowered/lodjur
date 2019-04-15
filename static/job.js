function subscribeToJobs(jobs) {
  console.log("subscribing to all job events");
  var stream = new EventSource('/jobs');

  stream.addEventListener('update', function (e) {
    var data = JSON.parse(e.data);
    var content = $(data.html);
    jobs.empty().append(content);
  });
};

function subscribeToJob(job) {
  var jobid = job.data('job-id');

  if (jobid) {
    console.log("subscribing to job events");
    var stream = new EventSource('/job/' + jobid + '/card');

    stream.addEventListener('update', function (e) {
      var data = JSON.parse(e.data);
      var content = $(data.html);
      job.empty().append(content);
    });
  };
};

function subscribeToLogs(logs) {
  var jobid = logs.data('job-id');

  if (jobid) {
    console.log("subscribing to log events");
    var stream = new EventSource('/job/' + jobid + '/logs');
  
    stream.addEventListener('logs', function (e) {
      var data = JSON.parse(e.data);
      data.data.forEach(function (line) {
        var l = document.createElement('div');
        l.classList.add('line');
	l.append(line)
        logs.append(l);
      });
    });
  };
};

$(document).ready(function() {
  var jobs = $("#jobs");
  if (jobs.length == 1) {
    subscribeToJobs(jobs);
  }
  var job = $("#job");
  if (job.length == 1) {
    subscribeToJob(job);
  }
  var logs = $("#logs");
  if (logs.length == 1) {
    subscribeToLogs(logs);
  }
});
