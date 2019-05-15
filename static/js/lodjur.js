$.fn.stream = function(streamfn, args = []) {
  this.each(function() {
    var self = $(this);
    var vals = args.map(a => self.data(a));
    s = streamfn.apply(null, vals);
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
