(function($) {
  var router = null;

  // ------------------------------------------------------------------------------------

  var NewSnippetView = Backbone.View.extend({
    events: {
      'click a.submit': 'saveSnippet'
    },

    render: function() {
      var html = $(router.renderTemplate('new-snippet', {post_url:'/snippet'}));
      $(this.el).html(html);
      return this.el;
    },

    saveSnippet: function(e) {
      e.preventDefault();

      var valid = this.validate('#title');
      valid = this.validate('#left') && valid;
      valid = this.validate('#right') && valid;

      if(valid) {
        $.post(this.$('form').attr('action'), this.serialize(), function(id) {
          router.goToSnippet(id);
        });
      }
    },

    validate: function(fieldId) {
      var field = this.$(fieldId);
      if(field.val().length == 0) {
        field.parent().parent().addClass('error');
        return false;
      }

      field.parent().parent().removeClass('error');
      return true;
    },

    serialize: function() {
      return {
        title: $.trim(this.$('#title').val()),
        left: $.trim(this.$('#left').val()),
        right: $.trim(this.$('#right').val())
      };
    }
  });

  // ------------------------------------------------------------------------------------

  var VoteActionsModel = Backbone.Model.extend({
    vote: function(which, left, right) {
      this.set('voted', which);
      this.set('left', left);
      this.set('right', right);
    },

    toJSON: function() {
      return {
        voted: $.trim(this.get('voted')).length > 0,
        voted_left: $.trim(this.get('voted')) === "left",
        voted_right: $.trim(this.get('voted')) === "right",
        key: $.trim(this.get('key')),
        show_actions: router.isUserSignedIn()
      };
    }
  });

  var VoteActionsView = Backbone.View.extend({
    events: {
      'click a.vote-left': 'voteLeft',
      'click a.vote-right': 'voteRight'
    },

    render: function() {
      var html = router.renderTemplate('vote-actions', this.model.toJSON());
      $(this.el).html(html);
      return this.el;
    },

    voteLeft: function(e) {
      this.vote('left', e);
    },

    voteRight: function(e) {
      this.vote('right', e);
    },

    vote: function(which, e) {
      var self = this;
      e.preventDefault();
      var postData = {
        snippet: $(e.target).data('key'),
        which: which
      };

      $.post("/vote", postData, function(result) {
        self.model.vote(which);
        self.$('.vote-buttons').hide();
        self.$('.vote-complete').show();

        self.trigger('votes:updated', {
          left: result.left,
          right: result.right
        });
      });
    }
  });

  // ------------------------------------------------------------------------------------

  var VotesModel = Backbone.Model.extend({
    toJSON: function() {
      return {
        left: this.get('left'),
        right: this.get('right')
      };
    }
  });

  var VotesView = Backbone.View.extend({
    votesUpdated: function(votes) {
      var self = this;

      self.model.set('left', votes.left);
      self.model.set('right', votes.right);

      var total = votes.left + votes.right;
      var leftRatio = votes.left / total * 100;
      var rightRatio = votes.right / total * 100;

      var winner = votes.left > votes.right
        ? "left"
        : (votes.left < votes.right ? "right" : "");

      this.refresh(winner, 'left', votes.left, leftRatio);
      this.refresh(winner, 'right', votes.right, rightRatio);
    },

    refresh: function(winner, side, votes, ratio) {
      self.$('#' + side + '_votes').text('?');

      var onFinish = function() {
        self.$('#' + side + '_votes').text(votes)
          .removeClass('badge-success')
          .removeClass('badge-important')
          .removeClass('badge-info')
          .addClass(winner.length > 0
            ? (winner === side ? 'badge-success' : 'badge-important')
            : 'badge-info');

        var parent = self.$('#' + side + '_ratio').parent()
          .removeClass('progress-success')
          .removeClass('progress-danger');

        if(winner.length > 0) {
          parent.addClass(winner === side ? 'progress-success' : 'progress-danger');
        }
      };

      if(votes > 0) {
        self.$('#' + side + '_ratio').animate({width: ratio + '%'},
            'slow', 'linear', onFinish);
      } else {
        onFinish();
      }
    },

    render: function() {
      var self = this;
      var html = router.renderTemplate('votes', self.model.toJSON());
      $(self.el).html(html);

      _.delay(function() {
        self.votesUpdated(self.model.toJSON());
      }, 500);

      return self.el;
    }
  });

  // ------------------------------------------------------------------------------------

  var SnippetModel = Backbone.Model.extend({
    initialize: function(spec) {
      this.votes = new VotesModel({
        left: spec.count.left,
        right: spec.count.right
      });

      this.actions = new VoteActionsModel({
        voted: spec.count.which,
        key: spec.snippet.key
      });

      this.set('left', spec.snippet.left);
      this.set('right', spec.snippet.right);
      this.set('title', spec.snippet.title);
    },

    toJSON: function() {
      return {
        left: $.trim(this.get('left')),
        right: $.trim(this.get('right')),
        title: $.trim(this.get('title')),
        key: this.get('key')
      };
    }
  });

  var SnippetView = Backbone.View.extend({
    votesUpdated: function(votes) {
      this.votes.votesUpdated(votes);
    },

    initialize: function(spec) {
      this.actions = new VoteActionsView({model: spec.model.actions});
      this.votes = new VotesView({model: spec.model.votes});
      this.actions.on("votes:updated", this.votesUpdated, this);
    },

    render: function() {
      var html = router.renderTemplate('snippet', this.model.toJSON());
      $(this.el).html(html).append(this.votes.render())
        .append(this.actions.render());

      return this.el;
    }
  });

  // ------------------------------------------------------------------------------------

  var LoginView = Backbone.View.extend({
    render: function() {
      var html = router.renderTemplate('login', {login_url: "/oauth/request"});
      $(this.el).html(html);
      return this.el;
    }
  });

  // ------------------------------------------------------------------------------------

  var UserModel = Backbone.Model.extend({
    initialize: function(spec) {
      this.set('user_id', spec.user_id);
      this.set('page', spec.page);
      this.set('pages', spec.pages);
      this.set('snippets', _.map(spec.snippets, function(s) {
        return {
          key: s.key,
          title: s.title,
          created: new Date(s.created).format('mmm dS @ HH:MM:ss')
        };
      }));
    },
    toJSON: function() {
      var pages = [];
      for (var i = 0; i < this.get('pages'); ++i) {
        pages.push({
          index: i,
          label: i + 1,
          current: i == this.get('page')
        });
      }

      return {
        is_current: this.get('is_current'),
        user_id: this.get('user_id'),
        user_name: this.get('user_name'),
        snippets: this.get('snippets'),
        has_pages: pages.length > 1,
        pages: pages
      };
    }
  });

  var UserView = Backbone.View.extend({
    render: function() {
      var json = this.model.toJSON();
      console.log(json);
      var html = router.renderTemplate('user-details', json);
      $(this.el).html(html);
      return this.el;
    }
  });

  // ------------------------------------------------------------------------------------

  window.CsdRouter = Backbone.Router.extend({
    routes: {
      'login': 'login',
      'user/:id': 'userHome',
      'user/:id/:page': 'userHome',
      'new-snippet': 'newSnippet',
      'snippet/:id': 'getSnippet',
      '': 'home'
    },

    setView: function(view, title) {
      var html = view.render();
      $('#content').html(html);

      var newTitle = "CodeSmackdown"
      if(!!title && title.length > 0) {
        newTitle += " - " + title;
      }
      document.title = newTitle;

      window.prettyPrint && prettyPrint();
    },

    logout: function() {
      self = this;
      $.post("/logoff", function() {
        location.reload();
      });
    },

    login: function() {
      if(!this.isUserSignedIn())
      {
        var view = new LoginView();
        this.setView(view, "Login");
      } else {
        this.home();
      }
    },

    home: function() {
      if(this.isUserSignedIn()) {
        this.navigate('user/' + this.currentUserId, {trigger: true, replace: true});
      } else {
        this.navigate('login', {trigger: true, replace: true});
      }
    },

    userHome: function(id, page) {
      var self = this;
      var path = '/userdetail/' + id;
      if (page) { path += '/' + page; }

      console.log(path);

      $.getJSON(path, function(user) {
        user.is_current = self.currentUserId == id;
        user.user_id = id;
        var model = new UserModel(user);
        var view = new UserView({model: model});
        self.setView(view, model.get('user_name'));
      });
    },

    newSnippet: function() {
      this.setView(new NewSnippetView(), 'New snippet');
    },

    getSnippet: function(id) {
      var self = this;
      $.getJSON('/snippet/' + id, function(snippet) {
        var model = new SnippetModel(snippet);
        var view = new SnippetView({model: model});
        self.setView(view, model.get('title'));
      });
    },

    goToSnippet: function(id) {
      this.navigate('snippet/' + id, {trigger: true, replace: true});
    },

    cache: {},

    loadTemplate: function(name) {
      var url = "/views/" + name + ".handlebars";
      var template = $.ajax({url: url, async: false}).responseText;
      return Handlebars.compile(template);
    },

    renderTemplate: function(name, data) {
      var self = this;
      if(!self.cache[name]) {
        self.cache[name] = self.loadTemplate(name);
      }

      return self.cache[name](data || {});
    },

    isUserSignedIn: function() {
      return this.currentUserId !== null
        && this.currentUserId !== undefined
        && this.currentUserId !== 0;
    },

    start: function(userId) {
      this.currentUserId = userId;
      router = this;

      Backbone.history.start();

      $(".logout").click(function(e) {
        e.preventDefault();
        router.logout();
      });

      var hash = window.location.hash;
      if(hash === '' || hash === '#') {
        if(!userId) {
          this.navigate('login', {trigger: true, replace: true});
        } else {
          this.navigate('user/' + userId, {trigger: true, replace: true});
        }
      }
    }
  });
})(jQuery);

