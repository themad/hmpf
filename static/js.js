(function($) {
    var jfhmpfMethods = {
        init: function(initData) {
            var $this = $(this),
                data = $this.data('jfhmpf');
            if(!data) {
                data = {
                    widgets: {
                        root: $this,
                        status: $this.find('.statusBox .status'),
                        list: $this.find('.listBox .list')
                    },
                    templates: {
                        listItem: $this.find('.listBox .list .listItem').detach()
                    },
                    results: {
                        status: null,
                        list: null
                    },
                    init: {}
                };
                if(typeof initData=='object') {
                    $.each(initData, function(k, v) { data.init[k] = v; });
                }
                $this.data('jfhmpf', data);

                $.each(['toggle', 'prev', 'next'], function(i, s) {
                    $this.find('.' + s).click(function() {$this.jfhmpf(s)});
                });

            }
            $.each(['status', 'list'], function(i, s) {
                if(data.widgets[s].length) $this.jfhmpf(s);
            });

            return this;
        },
        status: function() {
            var data = $(this).data('jfhmpf'),
                widgt = data.widgets.status;

            $.jpost('/status', {}, function(r) {
                data.results.status = r;
                $(this).data('jfhmpf', data);

                $.each(r, function(k, v) {
                    switch(k) {
                        case 'Time':
                            var prgrs = widgt.find('.TimeProgress'),
                                w = prgrs.width(),
                                done = v[0]/v[1];
                            prgrs.find('.done').css({width:(done*w) + 'px'});
                            prgrs.find('.notdone').css({width:((1-done)*w) + 'px'});
                            break;
                        case 'State':
                            var stt = widgt.find('.State');
                            $.each(v, function(k, v) {
                                stt.html(k);
                            });
                            break;
                        case 'Consume':case 'Random':case 'Repeat': case 'Single': case 'Consume':
                            widgt.find('.' + k).addClass(v ? 'true' : 'false')
                            break;
                        default:
                            widgt.find('.' + k).html(v);
                            break;
                    }
                });
            });
        },
        list: function() {
            var data = $(this).data('jfhmpf'),
                widgt = data.widgets.list,
                tmpl = data.templates.listItem,
                status = data.results.status;

            $.jget('/list/?count=10&start=0', function(r) {
                data.results.list = r;
                $(this).data('jfhmpf', data);

                widgt.find('.listItem').remove();

                $.each(r, function(i, idata) {
                    var itm = tmpl.clone();
                    idata.Tags.FilePath = idata.FilePath;
                    idata.Tags.Length = idata.Length;
                    $.each(idata.Tags, function(k, v) {
                        switch(k) {
                            default:
                                itm.find('.' + k).html(v);
                                break;
                        }
                    });
                    widgt.append(itm);
                });
            }, 'json');
        },
        toggle: function() {
            var $this=$(this);
            $.jpost('/toggle', {}, function() {

            });
            $this.jfhmpf('status');
        },
        prev: function() {
            var $this=$(this);
            $.jpost('/previous', {}, function() {

            });
            $this.jfhmpf('status');
        },
        next: function() {
            var $this=$(this);
            $.jpost('/next', {}, function() {

            });
            $this.jfhmpf('status');
        },
    };

    $.fn.extend({
        jfhmpf: function(method) {
            if(jfhmpfMethods[method]) {
                return jfhmpfMethods[method].apply(this, Array.prototype.slice.call(arguments, 1));
            } else if(typeof method === 'object' || !method) {
                return jfhmpfMethods.init.apply(this, arguments);
            } else {
                $.error( 'Method ' +  method + ' does not exist on jQuery.jfhmpf' );
            }    
        }
    });
})(jQuery);

$.extend({
    jget: function(url, func) {
        $.ajax({
            async: false,
            dataType: 'json',
            success: function(r) {
                func(r);
            },
            url: url
        });

        return this;
    },
    jpost: function(url, data, func) {
        $.ajax({
            async: false,
            type: 'POST',
            dataType: 'json',
            success: function(r) {
                func(r);
            },
            url: url,
            data: data
        });

        return this;
    },
});


$(function() {
    $('#jfhmpf').jfhmpf();
});