------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             BOARD READ                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with et_axes;						use et_axes;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_text;						use et_text;
with et_alignment;					use et_alignment;
with et_exceptions;					use et_exceptions;
with et_keywords;					use et_keywords;
with et_directions;					use et_directions;


package body et_board_read is

	
	procedure signal_layer_invalid (
		line			: in type_fields_of_line;
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
			"ERROR: " & get_affected_line (line) 
			& "Signal layer " & to_string (signal_layer) 
			& " is deeper than the deepest signal layer " 
			& to_string (check_layers.deepest_layer) & " !";
	end signal_layer_invalid;


	
	
	function to_layers (
		line			: in type_fields_of_line; -- layers 1 3 17
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
		place 		: type_field_count_positive := 2; -- we start reading the layer numbers with field 2

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
			while place <= get_field_count (line) loop

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
						"ERROR: " & get_affected_line (line) 
						& "Signal layer " & to_string (layer) 
						& " specified multiple times !";
				end if;
				
				place := place + 1; -- advance to next place
			end loop;
		
		end if;
		
		return layers;
	end to_layers;

	
	
-- BASIC GEOMETRIC OBJECTS USED IN PACKAGES AND BOARDS

	procedure board_reset_line is begin 
		reset_line (board_line); 
	end;

	
	procedure add_polygon_line (l : in out type_line) is begin
		append_segment (contour, (LINE, l));
		board_reset_line;
	end;	


	
	procedure board_reset_arc is begin 
		reset_arc (board_arc);
	end;
	

	
	procedure add_polygon_arc (a : in out type_arc) is begin
		append_segment (contour, (ARC, a));
		board_reset_arc;
	end;

	
	procedure board_reset_circle is begin 
		reset_circle (board_circle);
	end;

	
	procedure add_polygon_circle (c : in out type_circle) is begin
		-- The global contour variable "mutates" so that the contours
		-- consist of a single circle:
		contour := (
			contour	=> (circular => true, others => <>),
			others	=> <>);

		-- From now on the contour consists of just a single circle.
		-- Any attempt to append a line or an arc causes a discriminant error.
		
		-- Assign the circle to the contour:
		set_circle (contour, c);
		board_reset_circle;
	end;


	
	procedure read_board_line (
		line : type_fields_of_line)
	is
		kw : constant string := f (line, 1);
		vm : type_vector_model;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			vm := to_vector_model (line, 2);
			set_A (board_line, vm);
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			vm := to_vector_model (line, 2);
			set_B (board_line, vm);
			
		else
			invalid_keyword (kw);
		end if;
	end;



	
	function read_board_line (
		line : type_fields_of_line)
		return boolean 
	is
		kw : constant string := f (line, 1);
		vm : type_vector_model;
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			vm := to_vector_model (line, 2);
			set_A (board_line, vm);
			return true;
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			vm := to_vector_model (line, 2);
			set_B (board_line, vm);
			return true;
		else
			return false;
		end if;
	end;


	
	procedure board_check_arc (
		log_threshold	: in type_log_level) is
		use et_string_processing;
	begin
		log (text => "checking arc ...", level => log_threshold);
		
		if not is_valid (board_arc) then
			invalid_arc;
		end if;
	end board_check_arc;

	

	
	-- Reads start and end point of the board_arc. If the statement is invalid then an error issued.
	procedure read_board_arc (line : type_fields_of_line) is
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			set_A (board_arc, to_vector_model (line, 2));

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			set_B (board_arc, to_vector_model (line, 2));
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (board_arc, to_vector_model (line, 2));

		elsif kw = keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			set_direction (board_arc, to_direction (f (line, 2)));
			
		else
			invalid_keyword (kw);
		end if;
	end read_board_arc;

	
	
	
	-- Reads start and end point of the board_arc. If the statement is invalid then it returns a false.
	function read_board_arc (line : type_fields_of_line) return boolean is
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			set_A (board_arc, to_vector_model (line, 2));

			return true;

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			set_B (board_arc, to_vector_model (line, 2));

			return true;
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (board_arc, to_vector_model (line, 2));

			return true;

		elsif kw = keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			set_direction (board_arc, to_direction (f (line, 2)));

			return true;
			
		else
			return false;
		end if;
	end read_board_arc;

	

	
	-- Reads center and radius of the board_circle. If the statement is invalid then an error issued.
	procedure read_board_circle (line : type_fields_of_line) is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (board_circle, to_vector_model (line, 2));
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			set_radius (board_circle, to_radius (f (line, 2)));
		else
			invalid_keyword (kw);
		end if;
	end;

	
	
	-- Reads center and radius of the board_circle. If the statement is invalid then it returns false.
	function read_board_circle (line : type_fields_of_line) return boolean is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			set_center (board_circle, to_vector_model (line, 2));

			return true;
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			set_radius (board_circle, to_radius (f (line, 2)));

			return true;			
		else
			return false;
		end if;
	end;


	

	procedure check_outline (
		polygon			: in type_contour;
		log_threshold	: in type_log_level) 
	is
		status : constant type_contour_status := is_closed (polygon);
	begin
		log (text => "checking outline ...", level => log_threshold);
		log_indentation_up;
		
		if status.closed then
			null;
		else
			log (WARNING, "Contour not properly closed at:" & to_string (status.gaps));
			-- CS: write implications and dangers !
		end if;

		log_indentation_down;
	end check_outline;
	

	
	
	procedure board_reset_signal_layer is 
		use et_pcb_stack;
	begin
		signal_layer := type_signal_layer'first;
	end;

	

	
	procedure board_reset_line_width is 
	begin 
		board_line_width := linewidth_fab_min;
	end;


	
	procedure board_reset_contour is
		use et_pcb_stack;
	begin
		-- Some properties have no meaning in packages as remarked below.
		
		-- reset contour:
		contour := (others => <>);
		-- NOTE: A contour by default consists of lines and arcs.

		fill_spacing		:= type_track_clearance'first;
		board_filled		:= filled_default;
		board_fill_style	:= fill_style_default;
		--board_hatching		:= (others => <>);
		board_easing 		:= (others => <>);
		
		pad_connection	:= type_pad_connection'first; -- board relevant only
		contour_priority		:= type_priority'first;  -- board relevant only
		polygon_isolation		:= type_track_clearance'first;
		polygon_width_min		:= type_track_width'first;

		signal_layer			:= type_signal_layer'first;  -- board relevant only

		relief_properties		:= (others => <>); -- board relevant only
	end;
	
	
end et_board_read;
