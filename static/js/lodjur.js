function scrollMaybe(self, scroll, fn) {
  var top;
  if (scroll === true) {
    top = self.prop('scrollHeight') - self.prop('clientHeight');
    if (Math.abs(self.scrollTop() - top) > 4) {
      scroll = false;
    }
  }
  fn(self);
  if (scroll === true) {
    top = self.prop('scrollHeight') - self.prop('clientHeight');
    self.scrollTop(top);
  }
}

$.fn.stream = function(streamfn, args, scroll) {
  if (args === undefined) { args = []; }
  this.each(function() {
    var self = $(this);
    var vals = args.map(a => self.data(a));
    var s = streamfn.apply(null, vals);
    s.addEventListener('message', function (e) {
      scrollMaybe(self, scroll, function () {
        self.append(e.data);
      });
    });
  });
  return this;
};

var timerPretty;

$.fn.loadApi = function(getfn, args, scroll) {
  var self = this;
  if (self.length === 0)
    return self;
  if (args === undefined) { args = []; }
  var cb = function(data) {
    window.clearTimeout(timerPretty);
    scrollMaybe(self, scroll, function () {
      self.html(data);
      updatePretty();
    });
  };
  var vals = args.map(a => self.data(a));
  vals.push(cb);
  getfn.apply(self, vals);
  return self;
};

function updateJobsCards() {
  $('.job-list').loadApi(getApiJobsCards);
  $('.job-list').on('click', '.card[data-job-id]', function() {
    window.open("/job/" + $(this).data('jobId'), "_self");
  });
}

function updateJobDetail(jobid) {
  $('.job-detail[data-job-id]').loadApi(getApiJobByJobIdCard, ['jobId']);
}

function updateJobLogs(jobid) {
  $('.job-log[data-job-id]').loadApi(getApiJobByJobIdLogs, ['jobId'], true);
}

function updatePretty() {
  var now = moment();
  var tdiffs = $('.time').map(updatePrettyTime(now)).get();
  var ddiffs = $('.duration').map(updatePrettyDuration(now)).get();
  var mindiff = Math.min(Infinity, ...tdiffs, ...ddiffs);
  if (mindiff < 60) {
    timerPretty = window.setTimeout(updatePretty, 1000);
  } else {
    timerPretty = window.setTimeout(updatePretty, 60000);
  }
}

function updatePrettyTime(now) {
  return function() {
    var self = $(this);
    var time = self.children('.utctime').text();
    var m = moment.utc(time);
    self.children('.time-pretty').text(m.from(now));
    return Math.abs(now.diff(m, 'seconds'));
  };
}

function updatePrettyDuration(now) {
  return function() {
    var self = $(this);
    var start = self.children('.duration-start').text();
    var end = self.children('.duration-end').text();
    var diff = Infinity;
    var m;
    if (end === '') {
      m = moment.duration(now.diff(moment.utc(start)));
      diff = Math.abs(m.asSeconds());
    } else {
      m = moment.duration(moment.utc(end).diff(moment.utc(start)));
    }
    self.children('.duration-pretty').text(m.humanize());
    return diff;
  };
}

$(document).ready(function() {
  moment.relativeTimeThreshold('M', 12);
  moment.relativeTimeThreshold('d', 30);
  moment.relativeTimeThreshold('h', 24);
  moment.relativeTimeThreshold('m', 60);
  moment.relativeTimeThreshold('s', 60);
  moment.relativeTimeThreshold('ss', 3);
  moment.relativeTimeRounding(Math.floor);
  var s = streamApiJobsWatch();
  s.addEventListener('message', function (e) {
    var data = JSON.parse(e.data);
    if (data.tag === 'JobUpdated') {
      updateJobsCards();
      updateJobDetail();
    }
  });

  updatePretty();
  updateJobsCards();
  updateJobDetail();
  $('.job-log[data-job-id]').stream(streamApiJobByJobIdWatchLogs, ['jobId'], true);
});
