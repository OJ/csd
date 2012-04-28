(function($) {
  var router = null;

  // ------------------------------------------------------------------------------------

  var NewSnippetView = Backbone.View.extend({
    events: {
      'click a.submit': 'saveSnippet'
    },

    render: function() {
      var html = router.renderTemplate('new-snippet', {post_url:'/snippet'});
      $(this.el).html(html);
      return this.el;
    },

    saveSnippet: function(e) {
      e.preventDefault();
      $.post(this.$('form').attr('action'), this.serialize(), function(id) {
        router.goToSnippet(id);
      });
    },

    serialize: function() {
      return {
        title: $.trim(this.$('input[name="title"]').val()),
        left: $.trim(this.$('textarea[name="left"]').val()),
        right: $.trim(this.$('textarea[name="right"]').val())
      };
    }
  });

  // ------------------------------------------------------------------------------------

  var SnippetModel = Backbone.Model.extend({
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
    events: {
      'click a.vote-left': 'voteLeft',
      'click a.vote-right': 'voteRight'
    },

    render: function() {
      var html = router.renderTemplate('snippet', this.model.toJSON());
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
        key: $(e).data('key'),
        which: which
      };
      //$.post("/vote", postData, function(result) {
        self.$('.vote-buttons').hide();
        self.$('.vote-complete').show();
      //}, 'json');
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
      this.set('snippets', _.map(spec.snippets, function(s) {
        return {
          key: s.key,
          title: s.title,
          created: new Date(s.created).format('mmm dS @ HH:MM:ss')
        };
      }));
    },
    toJSON: function() {
      return {
        is_current: this.get('is_current'),
        user_name: this.get('user_name'),
        snippets: this.get('snippets')
      };
    }
  });

  var UserView = Backbone.View.extend({
    render: function() {
      var html = router.renderTemplate('user-details', this.model.toJSON());
      $(this.el).html(html);
      return this.el;
    }
  });

  // ------------------------------------------------------------------------------------

  window.CsdRouter = Backbone.Router.extend({
    routes: {
      'login': 'login',
      'user/:id': 'userHome',
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

    login: function() {
      var view = new LoginView();
      this.setView(view, "Login");
    },

    home: function() {
      if(!!this.currentUserId) {
        this.navigate('user/' + this.currentUserId, {trigger: true, replace: true});
      } else {
        this.navigate('login', {trigger: true, replace: true});
      }
    },

    userHome: function(id) {
      var self = this;
      $.getJSON('/userdetail/' + id, function(user) {
        user.is_current = self.currentUserId == id;
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
      var url = "/static/views/" + name + ".handlebars";
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

    start: function(userId) {
      this.currentUserId = userId;
      router = this;

      Backbone.history.start();

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

