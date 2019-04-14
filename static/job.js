function subscribeToJobs(jobs) {
  console.log("subscribing to job events");
  var stream = new EventSource('/jobs');

  stream.addEventListener('update', function (e) {
    console.log("job event", e.data);
    var data = JSON.parse(e.data);
    var content = $(data.html);
    jobs.empty().append(content);
  });
};


$(document).ready(function() {
  var jobs = $("#jobs");
  if (jobs) {
    subscribeToJobs(jobs);
  }
});
