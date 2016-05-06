#directory "+xml-light";;
#load "xml-light.cma";;
#load "graphics.cma";;

type xml =  
  |Element of (string * (string * string) list * xml list)
  |PCData of string
;;

type node = {
  lat : float; 
  lon : float; 
  ntag : (string * string) list
} 
;;

type way = {
  refnodes : int list;
  wtag : (string * string) list
}
;;


let fosm = (Xml.parse_file (Sys.argv.(1)));;

(*on remplit les tables de hachage*)

let tableNode = Hashtbl.create 2000;;
let tableWay = Hashtbl.create 500;;

let listclefway = ref [];; 

let prnd elt = match elt with
  |Xml.Element("nd",l,_) -> int_of_string (List.assoc "ref" l)
  |_ -> 0
;;

let prtag elt = match elt with
  |Xml.Element("tag",l,_) -> ((List.assoc "k" l),(List.assoc "v" l)) 
  |_ -> ("rien","rien")
;;

let minlat = ref 0.;;
let minlon = ref 0.;;
let maxlat = ref 0.;;
let maxlon = ref 0.;;

let rec rempliTables elt = match elt with
  |Xml.Element("bounds",l,_) -> 
     minlat := (float_of_string (List.assoc "minlat" l));
     maxlat := (float_of_string (List.assoc "maxlat" l));
     minlon := (float_of_string (List.assoc "minlon" l));
     maxlon := (float_of_string (List.assoc "maxlon" l));
     ();
  |Xml.Element("osm",_,l) -> List.map rempliTables l;();
  |Xml.Element("node",l1,l2) -> Hashtbl.add tableNode (int_of_string (List.assoc "id" l1 )) {lat = (float_of_string (List.assoc "lat" l1));lon = (float_of_string (List.assoc "lon" l1)) ; ntag = (List.map prtag l2)};
  |Xml.Element("way",l1,l2) ->
     listclefway := (int_of_string (List.assoc "id" l1 ))::(!listclefway);
     Hashtbl.add tableWay (int_of_string (List.assoc "id" l1 )) {refnodes = (List.map prnd l2) ;wtag = (List.map prtag l2)} 
  |_ -> ()
;;
 
  
rempliTables fosm;;

(*fin*)


open Graphics;;

(* calcule de dimension *)

let dispr1lon = ref ((40000. /. 360.) *. (cos !minlat ));;
if !dispr1lon < 0. then dispr1lon := ((-1.) *. (!dispr1lon)) else ();;

let zoomh = 
  800. /. ((40000. /. 360.) *. (!maxlat -. !minlat))
;;

let zooml = 
  1400. /. (!dispr1lon *. (!maxlon -. !minlon))
;;


let getlatnodeconverti reff = 
  zoomh  *. ((40000. /. 360.) *. ((((Hashtbl.find tableNode (reff))).lat)  -. !minlat));
;;

let getlonnodeconverti reff = 
  !dispr1lon = ((40000. /. 360.) *. (cos (((Hashtbl.find tableNode (reff))).lat) ));
    zooml *. (!dispr1lon *. ((((Hashtbl.find tableNode (reff))).lon)  -. !minlon));
;;

(*fin*)

open_graph " 1400x800";;


exception E;;

let rec enleve0 l = match l with
  |[]-> []
  |a::q when not(a=0) -> a::(enleve0 q)
  |a::q -> enleve0 q
;;

let rec dernierE l = match l with
  |[]-> raise E
  |[a] -> a
  |a::q  -> dernierE q
;;


let premierCommeDernier l = ((List.hd (enleve0 l)) = (dernierE (enleve0 l)));;

(*fonction d'affichage*)

let rec afficheHwRues lref =match lref with
    |[] -> ()
    |t::q when (not(t=0))->lineto (int_of_float (getlonnodeconverti t)) (int_of_float (getlatnodeconverti t));
	moveto (int_of_float (getlonnodeconverti t)) (int_of_float (getlatnodeconverti t));
	afficheHwRues q
    |_-> ()
;;


let afficheHw hw lref = match hw with
  |"tertiary" | "tertiary_link" ->  
     set_line_width 4;
      set_color (rgb 255 204 0);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"residential" ->    
     set_line_width 4;
      set_color (rgb 255 255 204 );
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"cycleway" -> 
    set_line_width 1;
      set_color (rgb 204 255 255 );
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"unclassified" -> 
     set_line_width 4;
      set_color (rgb 255 255 255);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"service" -> 
     set_line_width 3;
      set_color (rgb 255 102 102);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"footway" | "living_street" | "pedestrian" -> 
     set_line_width 2;
      set_color (rgb 255 153 0);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"primary" |"primary_link" -> 
     set_line_width 6;
      set_color (rgb 51 102 102);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"motorway" | "motorway_link" | "trunk" | "trunk_link" -> 
     set_line_width 6;
      set_color (rgb 0 51 51);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
 |"secondery" | "secondery_link" -> 
     set_line_width 5;
      set_color (rgb 102 204 204);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
 |"path" -> 
     set_line_width 4;
      set_color (rgb 204 102 0);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"raceway" -> 
     set_line_width 4;
      set_color red;
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"road" | "track" -> 
     set_line_width 4;
      set_color (rgb 255 102 0);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"ford" -> 
     set_line_width 4;
      set_color (rgb 51 102 255);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
 |"bridleway" -> 
     set_line_width 2;
      set_color (rgb 51 255 51);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
 |"bus_guideway" -> 
     set_line_width 4;
      set_color (rgb 0 0 0);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
 |"byway" -> 
     set_line_width 4;
      set_color (rgb 255 255 0);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |_->    set_line_width 6;
      set_color red;
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
;;
 
let tabb = ref [];;
let rec array_of_list l a n=match l with 
  |[] -> a
  | r::q -> Array.set a n r; array_of_list q a (n+1)
;;  
let rec remplilistepoint lref = match lref with
    |[] -> !tabb
    |[t] when (not(t=0))->tabb := (( int_of_float (getlonnodeconverti t)),(int_of_float (getlatnodeconverti t)))::!tabb;!tabb	 
    |t::q when (not(t=0))-> tabb := (((int_of_float (getlonnodeconverti t)),(int_of_float (getlatnodeconverti t)))::!tabb);
	remplilistepoint q
    |t::q when t = 0 -> remplilistepoint q 
;; 
let afficheNltout lref = 
  let t = remplilistepoint lref in begin 
  fill_poly (array_of_list t (Array.create (List.length t) (0,0)) 0);
      set_line_width 1;
      set_color (rgb 0 0 0);
  draw_poly (array_of_list t (Array.create (List.length t) (0,0)) 0) end;
    tabb := [];;

let afficheNt nt lref = match nt with
  |"bay" ->  
      set_color (rgb 255 204 0);
      afficheNltout lref
  |"cliff" | "coastline" ->    
     set_line_width 4;
      set_color (rgb 102 51 51);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"glacier" -> 
      set_color (rgb 204 255 255 );
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"heath" | "mud" -> 
      set_color (rgb 153 51 0);
      afficheNltout lref 
  |"beach" ->  
      set_color (rgb 255 204 102);
      afficheNltout lref 
  |"wetland" ->    
      set_color (rgb 102 204 204 );
      afficheNltout lref 
  |"scrub" -> 
      set_color (rgb 51 255 51);
      afficheNltout lref 
  |"water" -> 
      set_color (rgb 51 51 255);
      afficheNltout lref 
  |"wood" ->  
     set_color (rgb 0 102 0);
      afficheNltout lref 
  |_ ->set_color (rgb 200 200 200);
      afficheNltout lref

;;


let afficheLs ls lref = match ls with
  |"water_park" | "swimming_pool" | "fishing" ->  
      set_color (rgb 102 204 204);
      afficheNltout lref 
  |"common" | "garden" | "golf_course" | "nature_reserve" | "park" ->    
      set_color (rgb 51 204 0);
      afficheNltout lref 
  |_ -> 
      set_color (rgb 102 255 153);
      afficheNltout lref 

;;


let afficheLd ld lref = match ld with
  |"allotments" | "cemetery" | "forest" | "grass" | "greenhouse_horticulture" -> 
      set_color (rgb 0 102 0);
      afficheNltout lref 
  |"orchard" | "plant_nursery" | "recreation_ground" | "village_green" | "vineyard" ->  
      set_color (rgb 0 102 0);
      afficheNltout lref  
  |"basin" | "reservoir" ->    
     set_color (rgb 102 153 204);
      afficheNltout lref  
  |"construction" | "industrial"| "quarry" | "greenfield" | "brownfield" -> 
     set_color (rgb 51 51 0);
      afficheNltout lref 
  |"meadow" | "military" ->  
      set_color (rgb 255 204 102);
      afficheNltout lref 
  |"retail" ->    
      set_color (rgb 204 0 153 );
      afficheNltout lref 
  |"salt_pond" -> 
      set_color (rgb 153 204 255);
      afficheNltout lref 
  |"railway" -> 
      set_color (rgb 119 85 119);
      afficheNltout lref
  |"farmland" ->  
     set_color (rgb 255 204 0);
      afficheNltout lref 
  |"farm" -> 
      set_color (rgb 255 204 204 );
      afficheNltout lref 
  |"commercial" -> 
      set_color (rgb 255 255 153 );
      afficheNltout lref 
  |_ -> ()

;;


let afficheAw aw lref = match aw with
  |"aerodrome" | "helipad" ->  
      set_color (rgb 255 0 102);
      afficheNltout lref  
  |"runway" ->    
     set_line_width 6;
      set_color (rgb 0 0 51);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"apron" -> 
      set_color (rgb 102 0 255);
      afficheNltout lref 
  |"terminal" ->  
      set_color (rgb 153 102 102);
      afficheNltout lref 
  |_-> ()
;;


let afficheWw  ww lref = match ww with
  |"stream" | "drain" |"river" |"canal" | "ditch" ->    
     set_line_width 6;
      set_color (rgb 51 102 204); 
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"riverbank" -> 
     set_color (rgb 51 102 204); 
      let aff lref = 
	let t = remplilistepoint lref in begin 
	    fill_poly (array_of_list t (Array.create (List.length t) (0,0)) 0);
	    set_line_width 1;
	    set_color (rgb 51 102 204);
	    draw_poly (array_of_list t (Array.create (List.length t) (0,0)) 0) end;
	  tabb := []in
	aff lref 
  |"dock" | "boatyard" -> 
      set_color (rgb 0 153 153);
      afficheNltout lref 
  |_ -> ()
;;

let afficheRw rw lref = match rw with
  |"abandoned" | "construction" | "funiculaire" | "monorail" | "narrow_gauge" | "preserved" | "rail" ->
         set_line_width 1;
      set_color (rgb 0 0 0);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |"subway" -> 
             set_line_width 1;
      set_color (rgb 204 0 0);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
  |_ -> () 
;;


let afficheB lref =
      set_line_width 2;
      set_color (rgb 204 0 0);
      moveto  (int_of_float (getlonnodeconverti (List.hd lref))) (int_of_float (getlatnodeconverti (List.hd lref)));
      afficheHwRues (List.tl lref)
;;



let rec completetrace l rnode = match l with
  |[] -> ()  
  |(c,v)::q when ((c = "building" && v = "yes") || (c = "amenity")) -> set_color (rgb 119 85 119); (afficheNltout rnode); completetrace q rnode
  |(c,v)::q when (c = "building" && v = "greenhouse") -> set_color (rgb 0 153 0); (afficheNltout rnode);completetrace q rnode
  |(c,v)::q when c = "cycleway" -> (afficheHw c rnode);completetrace q rnode
  |(c,v)::q when c = "barrier" -> (afficheB rnode);completetrace q rnode 
  |(c,v)::q when c = "railway" -> (afficheRw v rnode);completetrace q rnode
  |(c,v)::q when c = "highway" -> (afficheHw v rnode);completetrace q rnode 
  |(c,v)::q when c = "natural" -> (afficheNt v rnode);completetrace q rnode  
  |(c,v)::q when c = "leisure" -> (afficheLs v rnode);completetrace q rnode
  |(c,v)::q when c = "landuse" -> (afficheLd v rnode);completetrace q rnode
  |(c,v)::q when c = "aeroway" -> (afficheAw v rnode);completetrace q rnode
  |(c,v)::q when c = "waterway" -> (afficheWw v rnode);completetrace q rnode
  |_::q -> completetrace q rnode
;;

set_color (rgb 200 200 200);;
fill_rect 0 0 1400 800;;

let rec trace l = match l with  
  |[] -> ()
  |t::q  when ((premierCommeDernier ((Hashtbl.find tableWay t).refnodes))) -> completetrace ((Hashtbl.find tableWay t).wtag) ((Hashtbl.find tableWay t).refnodes) ; trace q
  |t::q -> trace q
;;

let rec tracee l = match l with  
  |[] -> ()
  |t::q  when (not((premierCommeDernier ((Hashtbl.find tableWay t).refnodes)))) -> completetrace ((Hashtbl.find tableWay t).wtag) ((Hashtbl.find tableWay t).refnodes) ; tracee q
  |t::q -> tracee q
;;

trace !listclefway;;
tracee !listclefway;;

wait_next_event [Key_pressed];;
