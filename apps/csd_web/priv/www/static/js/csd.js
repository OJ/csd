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
        title: this.$('input[name="title"]').val(),
        left: this.$('textarea[name="left"]').val(),
        right: this.$('textarea[name="right"]').val()
      };
    }
  });

  // ------------------------------------------------------------------------------------

  var SnippetModel = Backbone.Model.extend({
    toJSON: function() {
      return {
        left: this.get('left'),
        right: this.get('right'),
        title: this.get('title')
      };
    }
  });

  var SnippetView = Backbone.View.extend({
    render: function() {
      var html = router.renderTemplate('snippet', this.model.toJSON());
      $(this.el).html(html);
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
      '': 'home',
      'login': 'login',
      'user/:id': 'userHome',
      'new-snippet': 'newSnippet',
      'snippet/:id': 'getSnippet'
    },

    setView: function(view) {
      var html = view.render();
      $('#content').html(html);
    },

    login: function() {
      var view = new LoginView();
      console.log(view);
      this.setView(view);
    },

    home: function() {
      if(!!this.currentUserId) {
        this.userHome(this.currentUserId);
      } else {
        this.login();
      }
    },

    userHome: function(id) {
      var self = this;
      $.getJSON('/userdetail/' + id, function(user) {
        user.is_current = self.currentUserId == id;
        var model = new UserModel(user);
        var view = new UserView({model: model});
        self.setView(view);
      });
    },

    newSnippet: function() {
      this.setView(new NewSnippetView());
    },

    getSnippet: function(id) {
      var self = this;
      $.getJSON('/snippet/' + id, function(snippet) {
        var model = new SnippetModel(snippet);
        var view = new SnippetView({model: model});
        self.setView(view);
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

      if(!userId) {
        this.navigate('login', {trigger: true, replace: true});
      } else {
        this.navigate('user/' + userId, {trigger: true, replace: true});
      }
    }
  });
})(jQuery);

