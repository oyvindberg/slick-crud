var no = no || {};
no.penger = no.penger || {};
no.penger.crud = no.penger.crud || {};

no.penger.crud.makeEditable = function(editable, onChange){
    var $editable = $(editable);
    var current;

    $editable.focus(function(){
        current = this.value;
    });

    $editable.blur(function(){
        if(current != this.value && onChange){
            onChange.call(this, this.value, current);
        }
    });

    $editable.keydown(function(event){
        if(event.which == 13){ //ENTER
            event.preventDefault();
            $(this).blur();
        } else if(event.which == 27){ // ESC
            event.preventDefault();
            $(this).text(current);
            $(this).blur();
        }
    })
};

no.penger.crud.single = function(url, root){

    function setup(){
        $(root + ' input[type!=checkbox]').each(function(i, e){
            no.penger.crud.makeEditable(e, function(text, old){
                 var elem   = $(this);
                 var id     = $(elem.closest('table')).attr('db-id');
                 var key    = elem.closest('tr').find('td:first').text();

                 var data  = {};
                 data[key] = text;
                 no.penger.crud.send(url + "/" + id, data, function(){
                     elem.text(old);
                 });
             });
        });

        $(root + ' input[type=checkbox]').click(function(){
            var self    = this;
            var elem   = $(this);
            var id     = $(elem.closest('table')).attr('db-id');
            var key    = elem.closest('tr').find('td:first').text();
            var checked = this.checked;
            var data    = {};
            data[key]   = checked;
            no.penger.crud.send(url + "/" + id, data, function(){
                self.checked = !checked;
            });
        });
    }

    $(setup)
};

no.penger.crud.neew = function(url, root){
    function setup(){
        $("#submit").click(submit);
        no.penger.crud.makeEditable($(root + ' [contenteditable=true]'), null);
    }

    function submit(){
        /* not terribly proud of this, but its late... :)*/
        var form = document.createElement("form");
        form.setAttribute("method", "POST");
        form.setAttribute("action", url + "/new");

        $(root).find('tr').each(function(index) {
            if (!index) return;
            var $self = $(this);

            var id    = $($self.find("td")[0]).text();
            var value = $self.find("input").val();

            var hiddenField = document.createElement("input");
            hiddenField.setAttribute("type", "hidden");
            hiddenField.setAttribute("name", id);
            hiddenField.setAttribute("value", value);

            form.appendChild(hiddenField);

        });


        document.body.appendChild(form);
        form.submit();

    }
    $(setup)
};

no.penger.crud.view = function(path, root){

    function setup(){
        no.penger.crud.makeEditable($(root + ' input[type!=checkbox]'), function(text, old){
            var elem   = $(this);
            var column = elem.closest('table').find('th:eq('+this.parentElement.cellIndex+')').text();
            var id     = $(elem.closest('tr')).attr('db-id');

            var data     = {};
            data[column] = text;

            no.penger.crud.send(path + "/" + id, data, function(){
                elem.text(old);
            });
        });

        $(root +' input[type=checkbox]').click(function(){
            var self   = this;
            var elem   = $(this);
            var column = elem.closest('table').find('th:eq('+this.parentElement.cellIndex+')').text();
            var id     = elem.closest('tr').find('td:first').text();

            var checked  = this.checked;
            var data     = {};
            data[column] = checked;

            no.penger.crud.send(path + "/" + id, data, function(){
                self.checked = !checked;
            })
        })
    }

    $(setup)
};

no.penger.crud.send = function(url, data, undo){
    $.ajax({
        url: url,
        type: 'post',
        data: $.param(data),
        success:function(msg){
            console.debug('success' + msg);
        },
        error:function(error){
            alert(error.responseText);
            undo();
        }
    });
};