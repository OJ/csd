(function($) {
  var router = null;

  // ------------------------------------------------------------------------------------

  var NewSnippetView = Backbone.View.extend({
    events: {
      'click a.submit': 'saveSnippet'
    },

    render: function() {
      var html = router.renderTemplate('#new-snippet-tmpl', {post_url:'/snippet'});
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
      var html = router.renderTemplate('#snippet-tmpl', this.model.toJSON());
      $(this.el).html(html);
      return this.el;
    }
  });

  // ------------------------------------------------------------------------------------

  var UserModel = Backbone.Model.extend({
    toJSON: function() {
      return {
        user_name: this.get('user_name'),
        snippets: this.get('snippets')
      };
    }
  });

  var UserView = Backbone.View.extend({
    render: function() {
      var html = router.renderTemplate('#user-tmpl', this.model.toJSON());
      $(this.el).html(html);
      return this.el;
    }
  });

  // ------------------------------------------------------------------------------------

  window.CsdRouter = Backbone.Router.extend({
    routes: {
      'logon': 'logon',
      'user/:id': 'userHome',
      'new-snippet': 'newSnippet',
      'snippet/:id': 'getSnippet'
    },

    setView: function(view) {
      $('#content').html(view.render());
    },

    logon: function() {
      this.setView(new LogonView());
    },

    userHome: function(id) {
      var self = this;
      $.getJSON('/userdetail/' + id, function(user) {
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

    cache: [],

    renderTemplate: function(id, data) {
      var self = this;
      if(!self.cache[id]) {
        var source = $(id).html();
        self.cache[id] = Handlebars.compile(source);
      }

      return self.cache[id](data || {});
    },

    start: function(userId) {
      router = this;

      Backbone.history.start();

      if(!userId) {
        this.navigate('logon', {trigger: true, replace: true});
      } else {
        this.navigate('user/' + userId, {trigger: true, replace: true});
      }
    }
  });
})(jQuery);

