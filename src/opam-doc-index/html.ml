let concat ?(sep="\n") = function
  | [] -> Cow.Html.nil
  | [h] -> h
  | h :: hs -> List.fold_left (fun l r -> <:html<$l$$str:sep$$r$>>) h hs

let string_of_html = Cow.Html.to_string

let html_of_string = Cow.Html.of_string ~enc:`UTF_8

let code ?cls data =
  match cls with
    None -> <:html<<code>$data$</code>&>>
  | Some css -> <:html<<code class="$str:css$">$data$</code>&>>

(* TODO: we default to adding odoccode here because it would require
   changing all the invocations in generate.ml to include it.  We
   should do that properly there and then remove this override. *)
let pre ?cls data =
  match cls with
    None -> <:html<<pre class="odoccode">$data$</pre>&>>
  | Some css -> <:html<<pre class="$str:css$">$data$</pre>&>>

let div data =
  <:html<<div>$data$</div>&>>

let id data =
  data
