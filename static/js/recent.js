var Recent = {
    contentLimit: 300,

    loop: function(idx) {
        var obj = this.recentData[idx]

        if (typeof obj === 'undefined') {
            if (idx != 0) {
                Recent.loop(0); // passed end, wrap around
            }

            return;
        }

        var landlord = obj.landlord
          , review   = obj.review
          , user     = obj.user;

        html = '<blockquote>'
             + '<p><strong>' + landlord.name + '</strong>: ' + review.content.substring(0, this.contentLimit) + '...</p>'
             + '<small>' + user.username + ' - <a href="' + review.link + '">Read more</a></small>'
             + '</blockquote>';

        $("#recent-reviews").fadeOut('fast', function() {
            $(this).html(html).fadeIn('fast');
        });

        idx++;
        setTimeout('Recent.loop(' + idx + ');', 10000);
    },

    init: function() {
        var self = this;

        $.getJSON("/recent", function(data) {
            self.recentData = data;
            self.loop(0);
        });
    }
};
