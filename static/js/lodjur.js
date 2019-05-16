/*
$.fn.stream = function(streamfn, args = []) {
  this.each(function() {
    var self = $(this);
    var vals = args.map(a => self.data(a));
    var s = streamfn.apply(null, vals);
    s.addEventListener('message', function (e) {
      self.html(e.data);
    });
  });
  return this;
};

$(document).ready(function() {
  $("#recent-jobs").stream(streamApiJobsWatch);
  $("#job").stream(streamApiJobByJobIdWatch, ['job-id']);
  $("#logs").stream(streamApiJobByJobIdWatchLogs, ['job-id']);
});
*/

$.fn.loadApi = function(fn) {
  var self = this;
  fn( function(data) {
    self.html(data);
  } );
  return this;
};

function updateJobsOutline() {
  $(".jobs-outline").loadApi(getApiJobsoutline);
}

$(document).ready(function() {
  var s = streamApiJobsWatch();
  s.addEventListener('message', function (e) {
    var data = JSON.parse(e.data);
    if (data.tag === "JobUpdated") { updateJobsOutline(); }
  });

  updateJobsOutline();
});