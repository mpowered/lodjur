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

var timerPretty;

$.fn.loadApi = function(fn) {
  var self = this;
  fn( function(data) {
    window.clearTimeout(timerPretty);
    self.html(data);
    updatePretty();
  } );
  return this;
};

function openJobDetail(jobId) {
  updateJobDetail(jobId);
  updateJobLogs(jobId);
}

function updateJobsOutline() {
  $('.jobs-outline').loadApi(getApiJobsOutline);
  $('.jobs-outline').on('click', '.job', function() {
    $('.jobs-outline .job').removeClass('active');
    $(this).addClass('active');
    openJobDetail($(this).data('jobId'));
  });
}

function updateJobDetail(jobId) {
  $('.job-detail').loadApi(_.partial(getApiJobByJobIdDetail, jobId));
}

function updateJobLogs(jobId) {
  $('.job-log').loadApi(_.partial(getApiJobByJobIdLogs, jobId));
}

function updateJobsCards() {
  $('.card-list').loadApi(getApiJobsCards);
}

function updatePretty() {
  var now = moment();
  var tdiffs = $('.time').map(updatePrettyTime(now)).get();
  var ddiffs = $('.duration').map(updatePrettyDuration(now)).get();
  var mindiff = Math.min(1000, ...tdiffs, ...ddiffs);
  if (mindiff < 60000) {
    timerPretty = window.setTimeout(updatePretty, 1000);
  } else {
    timerPretty = window.setTimeout(updatePretty, 60000);
  }
}

function updatePrettyTime(now) {
  return function() {
    var self = $(this);
    var time = self.children('.utctime').text();
    var m = moment(time);
    self.children('.time-pretty').text(m.from(now));
    return Math.abs(now.diff(m, 'seconds'));
  };
}

function updatePrettyDuration(now) {
  return function() {
    var self = $(this);
    var start = self.children('.duration-start').text();
    var end = self.children('.duration-end').text();
    if (end === undefined) { end = now; }
    var m = moment.duration(moment(end).diff(moment(start)));
    self.children('.duration-pretty').text(m.humanize());
    return Math.abs(m.asSeconds());
  };
}

$(document).ready(function() {
  moment.relativeTimeThreshold('M', 12);
  moment.relativeTimeThreshold('d', 30);
  moment.relativeTimeThreshold('h', 24);
  moment.relativeTimeThreshold('m', 90);
  moment.relativeTimeThreshold('s', 90);
  moment.relativeTimeThreshold('ss', 15);
  var s = streamApiJobsWatch();
  s.addEventListener('message', function (e) {
    var data = JSON.parse(e.data);
    if (data.tag === 'JobUpdated') { updateJobsOutline(); }
    if (data.tag === 'JobUpdated') { updateJobsCards(); }
  });

  updatePretty();
  updateJobsOutline();
  updateJobsCards();
});