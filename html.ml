let concat =
  List.fold_left
    (fun acc elem ->
      <:html<$acc$
$elem$>>) Cow.Html.nil

let string_of_html = Cow.Html.to_string

let html_of_string = Cow.Html.of_string ~enc:`UTF_8

let rec insert_between sep = function
  | [] -> Cow.Html.nil
  | [h] -> h
  | h::t -> <:html<$h$$str:sep$$insert_between sep t$>>

let code ?cls data =
  match cls with
    None -> <:html<<code>$data$</code>&>>
  | Some css -> <:html<<code class="$str:css$">$data$</code>&>>

let pre data =
  <:html<<pre>$data$</pre>&>>
