var dropdownBinding = new Shiny.InputBinding();
$.extend(dropdownBinding, {
  find: function(scope) {
    return $(scope).find('.btn.dropdown-toggle');
  },
  getValue: function(el) {
    return $(el).parent().hasClass("show");
  },
  setValue: function(el, value) {
    $(el).dropdown('toggle');
  },
  receiveMessage: function(el, data) {
    this.setValue(el, data);
    $(el).trigger('change');
  },
  subscribe: function(el, callback) {
    // need to target the first parent
    $(el).closest('.btn-group').on('shown.bs.dropdown.dropdownBinding', function(e) {
      callback();
    });

    // need to target the first parent
    $(el).closest('.btn-group').on('hidden.bs.dropdown.dropdownBinding', function(e) {
      callback();
    });

    $(el).on('change.dropdownBinding', function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.dropdownBinding');
  }
});

Shiny.inputBindings.register(dropdownBinding, 'shiny.dropdown');
