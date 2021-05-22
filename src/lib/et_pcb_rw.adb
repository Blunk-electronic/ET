------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PCB READ AND WRITE                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	--use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;
with ada.tags;

with ada.exceptions;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;				use et_general;
with et_string_processing;
with et_general_rw;				use et_general_rw;
with et_text;					use et_text;
with et_exceptions;				use et_exceptions;

package body et_pcb_rw is

	procedure write_text_properties (t : in et_packages.type_text'class) is
		use et_packages;
	begin
-- 		write (keyword => keyword_position, parameters => position (text.position) & 
-- 			space & keyword_rotation & to_string (get_angle (text.position))
-- 			  ); -- position x 0.000 y 5.555 rotation 0.00

		write (keyword => keyword_position, parameters => position (t.position));
			-- position x 0.000 y 5.555 rotation 0.00
		
		write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		write (keyword => keyword_line_width, parameters => to_string (t.line_width));
		write (keyword => keyword_alignment, parameters =>
			keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
			keyword_vertical   & space & to_string (t.alignment.vertical));
		
		-- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	end write_text_properties;

	procedure write_text_properties_with_face (
		t		: in et_packages.type_text'class;
		face	: in et_pcb_coordinates.type_face) 
		is
		use et_packages;
	begin
		write (keyword => keyword_position, parameters => position (t.position) & 
			space & keyword_face & to_string (face)); -- position x 0.000 y 5.555 rotation 0.00 face top

		-- CS this could be more elegant way. did not get it working
		-- 		write (keyword => keyword_position, parameters => 
		-- 			   position (type_position (text.position with face => face))
		-- 			  );
		
		write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		write (keyword => keyword_line_width, parameters => to_string (t.line_width));
		write (keyword => keyword_alignment, parameters =>
				keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
				keyword_vertical   & space & to_string (t.alignment.vertical)
				);
		-- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	end write_text_properties_with_face;

	procedure write_text (cursor : in et_packages.pac_texts_with_content.cursor) is
		use et_packages.pac_texts_with_content;
	begin
		text_begin;
		write (keyword => keyword_content, wrap => true,
			parameters => to_string (element (cursor).content));
		write_text_properties (element (cursor));
		text_end;
	end write_text;
	
	procedure write_width (width : in type_track_width) is begin
		write (keyword => keyword_width, parameters => to_string (width));
	end;

	procedure write_line (line : in pac_shapes.type_line'class) is begin
		write (keyword => keyword_start, parameters => position (line.start_point));
		write (keyword => keyword_end  , parameters => position (line.end_point));
	end write_line;

	procedure write_arc (arc : in pac_shapes.type_arc'class) is begin
		write (keyword => keyword_center, parameters => position (arc.center));
		write (keyword => keyword_start, parameters => position (arc.start_point));
		write (keyword => keyword_end, parameters => position (arc.end_point));
		write (keyword => et_geometry.keyword_direction, parameters => to_string (arc.direction));		
	end write_arc;

	procedure write_circle (circle : in pac_shapes.type_circle'class) is begin
		write (keyword => keyword_center, parameters => position (circle.center));
		write (keyword => keyword_radius, parameters => to_string (circle.radius));
	end write_circle;

	
	procedure write_hatching (hatching : in et_packages.type_hatching) is
		use et_packages;
	begin
		write (keyword => keyword_hatching_line_width  , parameters => to_string (hatching.line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (hatching.spacing));
		write (keyword => keyword_hatching_border_width, parameters => to_string (hatching.border_width));
	end;

	procedure write_hatching (hatching : in et_packages.type_conductor_hatching) is
		use et_packages;
	begin
		write (keyword => keyword_hatching_line_width  , parameters => to_string (hatching.line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (hatching.spacing));
		write (keyword => keyword_hatching_border_width, parameters => to_string (hatching.border_width));
	end;
	
	procedure write_easing (easing: in et_packages.type_easing) is
		use et_packages;
	begin
		write (keyword => keyword_easing_style, parameters => to_string (easing.style));
		write (keyword => keyword_easing_radius, parameters => to_string (easing.radius));
	end;

	procedure write_thermal (thermal : in type_thermal) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_technology, parameters => to_string (thermal.technology));
		-- CS might be a useless feature. inherited from kicad.
		
		write (keyword => keyword_thermal_width , parameters => to_string (thermal.width));
		write (keyword => keyword_thermal_gap   , parameters => to_string (thermal.gap));	
	end;

	procedure write_width_min (width : in type_track_width) is begin
		write (keyword => keyword_min_width, parameters => to_string (width));
	end;

	procedure write_isolation (iso : in type_track_clearance) is begin
		write (keyword => keyword_isolation, parameters => to_string (iso));
	end;

	procedure write_priority (prio : in type_polygon_priority) is
		use et_pcb;
	begin
		write (keyword => keyword_priority , parameters => to_string (prio));
	end;

	procedure write_signal_layer (layer : in et_pcb_stack.type_signal_layer) is 
		use et_pcb_stack;
	begin
		write (keyword => keyword_layer, parameters => to_string (layer));
	end;

	procedure write_fill_style (fill_style : in et_packages.type_fill_style) is
		use et_packages;
	begin
		write (keyword => keyword_fill_style, parameters => to_string (fill_style));
	end;

	procedure write_fill_status (filled : in type_filled) is begin
		write (keyword => keyword_filled, parameters => to_string (filled));
	end;
	
	procedure write_pad_connection (connection : in type_polygon_pad_connection) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_connection, parameters => to_string (connection));
	end;

	procedure write_pad_technology (techno : in type_polygon_pad_technology) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_technology, parameters => to_string (techno));
	end;	

	procedure write_signal_layers (layers : in et_pcb_stack.type_signal_layers.set) is
		use et_pcb_stack;
	begin
		write (keyword => keyword_layers, parameters => to_string (layers));
	end;
	
	procedure write_circle_fillable (circle : in type_fillable_circle) is begin
		circle_begin;
		write_circle (circle);
		write (keyword => keyword_filled, parameters => space & to_string (circle.filled));
		case circle.filled is
			when NO =>
				write (keyword => keyword_width, parameters => to_string (circle.border_width));
				
			when YES =>
				write (keyword => keyword_fill_style, parameters => space & to_string (circle.fill_style));

				case circle.fill_style is
					when SOLID => null;
					when HATCHED =>
						write (keyword => keyword_hatching_line_width  , parameters => to_string (circle.hatching.line_width));
						write (keyword => keyword_hatching_line_spacing, parameters => to_string (circle.hatching.spacing));
				end case;

		end case;
		circle_end;
	end write_circle_fillable;

	-- CS unify the follwing two procedures write_circle_conductor:
	procedure write_circle_conductor (circle : in type_conductor_circle) is begin
		circle_begin;
		write_circle (circle);
		write (keyword => keyword_filled, parameters => space & to_string (circle.filled));
		case circle.filled is
			when NO =>
				write (keyword => keyword_width, parameters => to_string (circle.border_width));
				
			when YES =>
				write (keyword => keyword_fill_style, parameters => space & to_string (circle.fill_style));

				case circle.fill_style is
					when SOLID => null;
					when HATCHED =>
						write (keyword => keyword_hatching_line_width  , parameters => to_string (circle.hatching.line_width));
						write (keyword => keyword_hatching_line_spacing, parameters => to_string (circle.hatching.spacing));
				end case;

		end case;
		circle_end;
	end write_circle_conductor;

	procedure write_circle_conductor (circle : in et_pcb.type_conductor_circle) is begin
		circle_begin;
		write_circle (circle);
		write_signal_layer (circle.layer);

		-- the signal layer:
		write (keyword => keyword_filled, parameters => space & to_string (circle.filled));
		
		case circle.filled is
			when NO =>
				write (keyword => keyword_width, parameters => to_string (circle.border_width));
				
			when YES =>
				write (keyword => keyword_fill_style, parameters => space & to_string (circle.fill_style));

				case circle.fill_style is
					when SOLID => null;
					when HATCHED =>
						write (keyword => keyword_hatching_line_width  , parameters => to_string (circle.hatching.line_width));
						write (keyword => keyword_hatching_line_spacing, parameters => to_string (circle.hatching.spacing));
				end case;

		end case;
		circle_end;
	end write_circle_conductor;

	
	procedure write_polygon_segments (
		polygon : in pac_shapes.type_polygon_base'class)
	is
		use pac_shapes.pac_polygon_segments;
		
		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
			case element (c).shape is
				
				when LINE =>
					line_begin;
					write_line (element (c).segment_line);
					line_end;

				when ARC =>
					arc_begin;
					write_arc (element (c).segment_arc);
					arc_end;

			end case;
		end query_segment;		

		contours : type_polygon_segments := get_segments (polygon);
		
	begin				
		if contours.circular then

			circle_begin;
			write_circle (contours.circle);
			circle_end;

		else
			contours.segments.iterate (query_segment'access);
		end if;
		
	end write_polygon_segments;
	


	
	function to_position ( -- CS combine with next function to_position using the tag test ?
	-- Returns a type_point_2d in the the layout.
		line : in et_string_processing.type_fields_of_line; -- "start x 44.5 y 53.5"
		from : in count_type)
		return type_point 
	is
		use et_string_processing;

		point : type_point; -- to be returned
		place : count_type := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= field_count (line) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				set (
					point	=> point,
					axis	=> X,
					value 	=> to_distance (f (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				set (
					point	=> point,
					axis 	=> Y,
					value 	=> to_distance (f (line, place + 1)));

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;
		
	function to_position (
	-- Returns a type_position in the layout.
		line : in et_string_processing.type_fields_of_line; -- "x 23 y 0.2 rotation 90.0"
		from : in count_type)
		return type_position 
	is
		use et_string_processing;
		
		point : type_position; -- to be returned
		place : count_type := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= field_count (line) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				set (point => point, axis => X, value => to_distance (f (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				set (point => point, axis => Y, value => to_distance (f (line, place + 1)));

			-- We expect after "rotation" the corresponding value for the rotation
			elsif f (line, place) = keyword_rotation then
				set (point, to_rotation (f (line, place + 1)));
				
			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;

	function position (point : in type_point'class) return string is
		use ada.tags;

		xy : constant string := space & keyword_x & to_string (x (point)) 
				& space & keyword_y & to_string (y (point));
	begin
		if point'tag = type_point'tag then
			return xy;
			-- x 162.560 y 98.240
			
		elsif point'tag = type_position'tag then
			return xy 
				& space & keyword_rotation & to_string (rot (type_position (point)));
				-- x 162.560 y 98.240 rotation 180.00
			
		elsif point'tag = type_package_position'tag then
			return xy
				& space & keyword_rotation & to_string (rot (type_position (point)))
				& space & keyword_face & to_string (get_face (type_package_position (point)));
				-- x 162.560 y 98.240 rotation 180.00 face top
		else
			return xy;
		end if;

	end position;
	
	function to_grid (
		line : in et_string_processing.type_fields_of_line; -- "default x 1 y 1"
		from : in count_type)
		return type_grid 
	is
		use et_string_processing;
		
		grid : type_grid; -- to be returned

		place : count_type := from; -- the field being read from given line

	begin
		while place <= field_count (line) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				grid.x := to_distance (f (line, place + 1));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				grid.y := to_distance (f (line, place + 1));

			else
				invalid_keyword (f (line, place));
			end if;
					
			place := place + 2;
		end loop;
		
		return grid;
	end to_grid;

	procedure signal_layer_invalid (
		line			: in et_string_processing.type_fields_of_line;
		signal_layer	: in et_pcb_stack.type_signal_layer;
		check_layers	: in et_pcb_stack.type_layer_check) 
	is
		use et_string_processing;
		use et_pcb_stack;
	begin
		--log (WARNING, affected_line (line) & "Signal layer " & to_string (signal_layer) &
			 --" is deeper than the deepest signal layer " &
			 --to_string (check_layers.deepest_layer) & " !" &
		--" Objects in this layer will be ignored !");
		
		raise semantic_error_1 with
			"ERROR: " & affected_line (line) 
			& "Signal layer " & to_string (signal_layer) 
			& " is deeper than the deepest signal layer " 
			& to_string (check_layers.deepest_layer) & " !";
	end signal_layer_invalid;
	
	
	function to_layers (
		line			: in et_string_processing.type_fields_of_line; -- layers 1 3 17
		check_layers	: in et_pcb_stack.type_layer_check)
		return et_pcb_stack.type_signal_layers.set 
	is
		use et_pcb;
		use et_pcb_stack;
		use et_pcb_stack.type_signal_layers;
		use et_string_processing;

		layers 		: type_signal_layers.set; -- to be returned
		cursor 		: type_signal_layers.cursor;
		inserted	: boolean;
		layer 		: type_signal_layer;
		place 		: count_type := 2; -- we start reading the layer numbers with field 2

		field_2			: constant string := f (line, 2);
		field_2_first	: constant positive := field_2'first;
		field_2_last	: constant positive := field_2'last;

		procedure validate_layer (c : in type_signal_layers.cursor) is begin
			if not signal_layer_valid (element (c), check_layers) then
				signal_layer_invalid (line, element (c), check_layers);
			end if;
		end validate_layer;
		
	begin -- to_layers
		
		-- Test the first character of the 2nd field.
		-- If it is the start mark of a layer term like [1, 3, 6-11]
		-- then it must be converted to a set of layers.
		-- Otherwise we assume the layer numbers are given in a
		-- row of discrete layer ids like "1 4 10"
		if field_2 (field_2_first) = layer_term_start then

			layers := to_layers (field_2);

			-- Iterate layers and validate each of them.
			layers.iterate (validate_layer'access);
			
		else -- discrete layer ids like "1 4 10"
			while place <= field_count (line) loop

				-- get the layer number from current place
				layer := to_signal_layer (f (line, place));

				-- Issue warning if signal layer is invalid:
				if not signal_layer_valid (layer, check_layers) then
					signal_layer_invalid (line, layer, check_layers);
				end if;

				-- insert the layer number in the container "layers"
				insert (
					container	=> layers,
					new_item	=> layer,
					inserted	=> inserted,
					position	=> cursor);

				-- warn if layer already in container
				if not inserted then
					
					--log (WARNING, affected_line (line) & "signal layer " & to_string (layer) 
					--& " specified multiple times !");
					
					raise semantic_error_1 with
						"ERROR: " & affected_line (line) 
						& "Signal layer " & to_string (layer) 
						& " specified multiple times !";
				end if;
				
				place := place + 1; -- advance to next place
			end loop;
		
		end if;
		
		return layers;
	end to_layers;

	
	
-- BASIC GEOMETRIC OBJECTS USED IN PACKAGES AND BOARDS

	procedure board_reset_line is begin board_line := (others => <>); end;

	procedure add_polygon_line (l : in out type_line) is begin
		pac_shapes.append_segment (polygon, (LINE, l));
		board_reset_line;
	end;	

	procedure board_reset_arc is begin board_arc := (others => <>); end;
	
	procedure add_polygon_arc (a : in out type_arc) is begin
		pac_shapes.append_segment (polygon, (ARC, a));
		board_reset_arc;
	end;

	procedure board_reset_circle is begin board_circle := (others => <>); end;
	
	procedure add_polygon_circle (c : in out type_circle) is begin
		-- The global polygon variable "mutates" so that the contours
		-- consist of a single circle:
		polygon := (contours => (circular => true, others => <>));

		-- From now on the polygon consists of just a single circle.
		-- Any attempt to append a line or an arc causes a discriminant error.
		
		-- Assign the circle to the polygon contours:
		pac_shapes.set_circle (polygon, c);
		board_reset_circle;
	end;


	
	procedure read_board_line (
		line : et_string_processing.type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_line.start_point := to_position (line, 2);
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_line.end_point := to_position (line, 2);
			
		else
			invalid_keyword (kw);
		end if;
	end;

	function read_board_line (
		line : et_string_processing.type_fields_of_line)
		return boolean 
	is
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_line.start_point := to_position (line, 2);
			return true;
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_line.end_point := to_position (line, 2);
			return true;
		else
			return false;
		end if;
	end;

	procedure board_check_arc (
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
	begin
		log (text => "checking arc ...", level => log_threshold);

		if not is_valid (board_arc) then
			invalid_arc;
		end if;
	end board_check_arc;
	
	-- Reads start and end point of the board_arc. If the statement is invalid then an error issued.
	procedure read_board_arc (line : et_string_processing.type_fields_of_line) is
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_arc.start_point := to_position (line, 2);

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_arc.end_point := to_position (line, 2);
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_arc.center := to_position (line, 2);

		elsif kw = et_geometry.keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			board_arc.direction := to_direction (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end;

	-- Reads start and end point of the board_arc. If the statement is invalid then it returns a false.
	function read_board_arc (line : et_string_processing.type_fields_of_line) return boolean is
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_arc.start_point := to_position (line, 2);

			return true;

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_arc.end_point := to_position (line, 2);

			return true;
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_arc.center := to_position (line, 2);

			return true;

		elsif kw = et_geometry.keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			board_arc.direction := to_direction (f (line, 2));

			return true;
			
		else
			return false;
		end if;
	end;
	
	-- Reads center and radius of the board_circle. If the statement is invalid then an error issued.
	procedure read_board_circle (line : et_string_processing.type_fields_of_line) is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_circle.center := to_position (line, 2);
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			board_circle.radius := to_distance (f (line, 2));
		else
			invalid_keyword (kw);
		end if;
	end;

	-- Reads center and radius of the board_circle. If the statement is invalid then it returns false.
	function read_board_circle (line : et_string_processing.type_fields_of_line) return boolean is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_circle.center := to_position (line, 2);

			return true;
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			board_circle.radius := to_distance (f (line, 2));

			return true;
			
		else
			return false;
		end if;
	end;


	procedure check_outline (
		polygon			: in pac_shapes.type_polygon;
		log_threshold	: in et_string_processing.type_log_level) 
	is
		use et_string_processing;
		status : constant type_polygon_status := is_closed (polygon);
	begin
		log (text => "checking polygon outline ...", level => log_threshold);
		log_indentation_up;
		
		if status.closed then
			null;
		else
			log (WARNING, "Polygon not properly closed at:" & to_string (status.gaps));
			-- CS: write implications and dangers !
		end if;

		log_indentation_down;
	end check_outline;
	

	
	
	procedure board_reset_signal_layer is 
		use et_pcb_stack;
	begin
		signal_layer := type_signal_layer'first;
	end;

	procedure board_reset_lock_status is
		use et_pcb;
	begin
		board_lock_status := NO;
	end;

	procedure board_reset_line_width is 
		use et_packages;
	begin 
		board_line_width := type_general_line_width'first; 
	end;

	-- package and board relevant:	
	procedure board_reset_circle_fillable is begin 
		board_circle		:= (others => <>);
		board_filled		:= type_filled'first;
		board_fill_style	:= fill_style_default;
		board_hatching		:= (others => <>);
		
		board_reset_line_width;
	end;

	function to_fillable_circle (
	-- Composes a fillable circle from the given parameters. 
	-- Filled and fill_style are discriminants. Depending on them some parameters
	-- matter or not. See spec for type_fillable_circle.
		circle				: in pac_shapes.type_circle;
		filled				: in type_filled;
		fill_style			: in et_packages.type_fill_style;
		circumfence_width	: in et_packages.type_general_line_width;
		hatching			: in et_packages.type_hatching)
		return type_fillable_circle is

	begin -- to_fillable_circle
		case filled is
			when NO =>
				return (circle with
					filled			=> NO,
					fill_style		=> fill_style,
					border_width	=> circumfence_width);
				
			when YES =>
				case fill_style is
					when SOLID =>
						return (circle with
							filled		=> YES,
							fill_style	=> SOLID);

					when HATCHED =>
						return (circle with
							filled				=> YES,
							fill_style			=> HATCHED,
							hatching			=> hatching);

				end case;
		end case;
	end to_fillable_circle;
	
	function board_make_fillable_circle return type_fillable_circle is begin
		return to_fillable_circle (
			circle 				=> pac_shapes.type_circle (board_circle),
			filled				=> board_filled,
			fill_style			=> board_fill_style,
			circumfence_width	=> board_line_width,
			hatching			=> board_hatching);
	end;

	function board_make_fillable_circle_solid return type_fillable_circle_solid is begin
		return (pac_shapes.type_circle (board_circle) with board_filled);
	end;

	function board_make_conductor_circle return type_conductor_circle is begin
		case board_filled is
			when NO =>
				return (pac_shapes.type_circle (board_circle) with 
					filled			=> NO,
					fill_style		=> SOLID, -- don't care here
					border_width	=> board_line_width);

			when YES =>
				case board_fill_style is
					when SOLID =>
						return (pac_shapes.type_circle (board_circle) with 
							filled		=> YES,
							fill_style	=> SOLID);

					when HATCHED =>
						return (pac_shapes.type_circle (board_circle) with
							filled		=> YES,
							fill_style	=> HATCHED,
							hatching 	=> board_hatching_conductor);
				end case;
		end case;
	end;
	
	procedure board_reset_polygon is
	-- This procedure resets the global variable "polygon" to its default.
	-- This proecdure is used by both package and board parsing procedures 
	-- read_package and read_module_file.
	-- Some properties have no meaning in packages as remarked below.
		use et_pcb_stack;
	begin
		-- reset polygon:
		polygon := (others => <>);
		-- NOTE: A polygon by default consists of lines and arcs.
		
		board_filled		:= filled_default;
		board_fill_style	:= fill_style_default;
		board_hatching		:= (others => <>);
		board_easing 		:= (others => <>);
		
		polygon_pad_connection	:= type_polygon_pad_connection'first; -- board relevant only
		polygon_priority		:= type_polygon_priority'first;  -- board relevant only
		polygon_isolation		:= type_track_clearance'first;
		polygon_width_min		:= type_track_width'first;

		signal_layer			:= type_signal_layer'first;  -- board relevant only

		thermal					:= (others => <>); -- board relevant only
	end;

	procedure fill_zone_begin is begin section_mark (section_fill_zone, HEADER); end;
	procedure fill_zone_end   is begin section_mark (section_fill_zone, FOOTER); end;
	procedure cutout_zone_begin is begin section_mark (section_cutout_zone, HEADER); end;
	procedure cutout_zone_end   is begin section_mark (section_cutout_zone, FOOTER); end;
	procedure contours_begin is begin section_mark (section_contours, HEADER); end;
	procedure contours_end   is begin section_mark (section_contours, FOOTER); end;

-- SILK SCREEN
	procedure write_line (cursor : in pac_silk_lines.cursor) is 
		use pac_silk_lines;
	begin
		line_begin;
		write_line (element (cursor));		
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in pac_silk_arcs.cursor) is 
		use pac_silk_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in pac_silk_circles.cursor) is 
		use pac_silk_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in pac_silk_polygons.cursor) is 
		use pac_silk_polygons;
	begin
		fill_zone_begin;

		write_easing (element (cursor).easing);
		write_fill_style (element (cursor).fill_style);

		case element (cursor).fill_style is
			when HATCHED =>
				write_hatching (element (cursor).hatching);

			when others => null;
		end case;

		contours_begin;		
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;

	end write_polygon;

	procedure write_cutout (cursor : in pac_silk_cutouts.cursor) is 
		use pac_silk_cutouts;
	begin
		cutout_zone_begin;

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;

		cutout_zone_end;
	end;

-- ASSEMBLY DOCUMENTATION
	procedure write_line (cursor : in pac_doc_lines.cursor) is 
		use pac_doc_lines;
	begin
		line_begin;
		write_line (element (cursor));		
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in pac_doc_arcs.cursor) is 
		use pac_doc_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in pac_doc_circles.cursor) is
		use pac_doc_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in pac_doc_polygons.cursor) is 
		use pac_doc_polygons;
	begin
		fill_zone_begin;

		write_easing (element (cursor).easing);
		write_fill_style (element (cursor).fill_style);

		case element (cursor).fill_style is
			when HATCHED =>
				write_hatching (element (cursor).hatching);

			when others => null;
		end case;

		contours_begin;		
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in pac_doc_cutouts.cursor) is 
		use pac_doc_cutouts;
	begin
		cutout_zone_begin;

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;
	
-- KEEPOUT
	procedure write_line (cursor : in pac_keepout_lines.cursor) is
		use pac_keepout_lines;
	begin
		line_begin;
		write_line (element (cursor));
		line_end;
	end write_line;

	procedure write_arc (cursor : in pac_keepout_arcs.cursor) is 
		use pac_keepout_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		arc_end;
	end write_arc;
	
	procedure write_circle (cursor : in pac_keepout_circles.cursor) is 
		use pac_keepout_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in pac_keepout_polygons.cursor) is 
		use pac_keepout_polygons;
	begin
		fill_zone_begin;

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;

		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in pac_keepout_cutouts.cursor) is 
		use pac_keepout_cutouts;
	begin
		cutout_zone_begin;
		
		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

-- STOP MASK
	procedure write_line (cursor : in pac_stop_lines.cursor) is 
		use pac_stop_lines;
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in pac_stop_arcs.cursor) is 
		use pac_stop_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in pac_stop_circles.cursor) is 
		use pac_stop_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in pac_stop_polygons.cursor) is 
		use pac_stop_polygons;
	begin
		fill_zone_begin;
		write_easing (element (cursor).easing);
		write_fill_style (element (cursor).fill_style);
		
		if element (cursor).fill_style = HATCHED then
			write_hatching (element (cursor).hatching);
		end if;

		contours_begin;		
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in pac_stop_cutouts.cursor) is 
		use pac_stop_cutouts;
	begin
		cutout_zone_begin;

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

-- STENCIL (OR SOLDER PASTE MASK)
	procedure write_line (cursor : in pac_stencil_lines.cursor) is 
		use pac_stencil_lines;
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in pac_stencil_arcs.cursor) is 
		use pac_stencil_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in pac_stencil_circles.cursor) is 
		use pac_stencil_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in pac_stencil_polygons.cursor) is 
		use pac_stencil_polygons;
	begin
		fill_zone_begin;
		write_easing (element (cursor).easing);
		write_fill_style (element (cursor).fill_style);
					  
		if element (cursor).fill_style = HATCHED then
			write_hatching (element (cursor).hatching);
		end if;

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in pac_stencil_cutouts.cursor) is 
		use pac_stencil_cutouts;
	begin
		cutout_zone_begin;

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

-- ROUTE RESTRICT
	procedure write_line (cursor : in pac_route_restrict_lines.cursor) is 
		use et_pcb_stack;
		use pac_route_restrict_lines;
	begin
		line_begin;
		write_line (element (cursor));
		write_signal_layers (element (cursor).layers);
		line_end;
	end write_line;

	procedure write_arc (cursor : in pac_route_restrict_arcs.cursor) is 
		use et_pcb_stack;
		use pac_route_restrict_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));		
		write_signal_layers (element (cursor).layers);
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in pac_route_restrict_circles.cursor) is 
		use pac_route_restrict_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in pac_route_restrict_polygons.cursor) is 
		use pac_route_restrict_polygons;
	begin
		fill_zone_begin;
		write_signal_layers (element (cursor).layers);

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in pac_route_restrict_cutouts.cursor) is 
		use pac_route_restrict_cutouts;
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

-- VIA RESTRICT
	procedure write_line (cursor : in pac_via_restrict_lines.cursor) is 
		use et_pcb_stack;
		use pac_via_restrict_lines;
	begin
		line_begin;
		write_line (element (cursor));		
		write_signal_layers (element (cursor).layers);
		line_end;
	end write_line;

	procedure write_arc (cursor : in pac_via_restrict_arcs.cursor) is 
		use et_pcb_stack;
		use pac_via_restrict_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write_signal_layers (element (cursor).layers);
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in pac_via_restrict_circles.cursor) is 
		use et_pcb_stack;		
		use pac_via_restrict_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in pac_via_restrict_polygons.cursor) is 
		use et_pcb_stack;
		use pac_via_restrict_polygons;
	begin
		fill_zone_begin;
		write_signal_layers (element (cursor).layers);			

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in pac_via_restrict_cutouts.cursor) is 
		use pac_via_restrict_cutouts;
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);
		
		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

	
-- BOARD CONTOUR
	--procedure write_line (cursor : in et_pcb.pac_pcb_contour_lines.cursor) is 
		--use et_pcb;
		--use et_pcb.pac_pcb_contour_lines;
	--begin
		--line_begin;
		--write_line (element (cursor));
		--write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		--line_end;
	--end write_line;
	
	--procedure write_arc (cursor : in et_pcb.pac_pcb_contour_arcs.cursor) is 
		--use et_pcb;
		--use et_pcb.pac_pcb_contour_arcs;
	--begin
		--arc_begin;
		--write_arc (element (cursor));
		--write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		--arc_end;
	--end write_arc;

	--procedure write_circle (cursor : in et_pcb.pac_pcb_contour_circles.cursor) is 
		--use et_pcb;
		--use et_pcb.pac_pcb_contour_circles;
	--begin
		--circle_begin;
		--write_circle (element (cursor));
		--write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		--circle_end;
	--end write_circle;
	
end et_pcb_rw;
