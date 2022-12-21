------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             PACKAGES                                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.exceptions;

package body et_packages is
	
	
	function to_string (packge : in pac_package_name.bounded_string) return string is
	-- CS: provide a parameter that turns the preamble on/off
	begin
		return pac_package_name.to_string (packge);
	end to_string;

	function to_package_name (package_name : in string) return pac_package_name.bounded_string is
	begin
		return pac_package_name.to_bounded_string (package_name);
	end to_package_name;

	
	procedure check_package_name_length (packge : in string) is
	begin
		if packge'length > package_name_length_max then
			log (WARNING, "package name too long. Max. length is" 
				 & positive'image (package_name_length_max) & " !");
		end if;
	end check_package_name_length;

	
	procedure check_package_name_characters (
		packge		: in pac_package_name.bounded_string;
		characters	: in character_set := package_name_characters)
	is
		use pac_package_name;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => packge,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log (WARNING, "package name " & enclose_in_quotes (to_string (packge))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position));
		end if;
	end check_package_name_characters;




	
	function to_string (name : in pac_package_model_file_name.bounded_string) 
		return string is
	begin
		return pac_package_model_file_name.to_string (name);
	end;

	function to_file_name (name : in string) 
		return pac_package_model_file_name.bounded_string is
	begin
		return pac_package_model_file_name.to_bounded_string (name);
	end;






	procedure validate_pad_size (size : in type_distance) is
	begin
		if size not in type_pad_size then
			log (ERROR, "pad size invalid ! Allowed range is" 
				 & to_string (type_pad_size'first) & " .."
				 & to_string (type_pad_size'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_pad_size;
	
-- 	procedure validate_track_clearance (clearance : in et_pcb_coordinates.type_distance) is
-- 	-- Checks whether the given track clearance is in range of type_track_clearance.
-- 	begin
-- 		if clearance not in type_track_clearance then
-- 			log (ERROR, "track clearance invalid ! Allowed range is" 
-- 				 & to_string (type_track_clearance'first) & " .."
-- 				 & to_string (type_track_clearance'last),
-- 				 console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end validate_track_clearance;
-- 
-- 	procedure validate_track_width (track_width : in type_distance_positive) is
-- 	-- Checks whether the given width is in range of type_track_width.
-- 	begin
-- 		if track_width not in type_track_width then
-- 			log (ERROR, "track width invalid ! Allowed range is" 
-- 				 & to_string (type_track_width'first) & " .."
-- 				 & to_string (type_track_width'last),
-- 				 console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end validate_track_width;

-- 	procedure validate_restring_width (restring_width : in et_pcb_coordinates.type_distance) is
-- 	-- Checks whether the given restring width is in range of type_restring_width.	
-- 	begin
-- 		if restring_width not in type_restring_width then
-- 			log (ERROR, "restring width invalid ! Allowed range is" 
-- 				 & to_string (type_restring_width'first) & " .."
-- 				 & to_string (type_restring_width'last),
-- 				 console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end validate_restring_width;


	
	function to_string (text_meaning : in type_text_meaning_package) return string is begin
		return to_lower (type_text_meaning_package'image (text_meaning));
	end;

	function to_text_meaning (text_meaning : in string) return type_text_meaning_package is begin
		return type_text_meaning_package'value (text_meaning);
	end;

	function to_string (layer : in type_placeholder_package_layer) return string is begin
		return to_lower (type_placeholder_package_layer'image (layer));
	end;

	function to_layer (layer : in string) return type_placeholder_package_layer is begin
		return type_placeholder_package_layer'value (layer);
	end;


	

-- 	procedure log_plated_millings (
-- 		millings 		: in type_plated_millings;
-- 		log_threshold	: in type_log_level)
-- 		is
-- -- 		use pac_pcb_contour_lines;
-- -- 		use pac_pcb_contour_arcs;
-- -- 		use pac_pcb_contour_circles;
-- -- 		
-- -- 		procedure line (cursor : in pac_pcb_contour_lines.cursor) is begin
-- -- 			line_pcb_contour_properties (cursor, log_threshold);
-- -- 		end;
-- -- 
-- -- 		procedure arc (cursor : in pac_pcb_contour_arcs.cursor) is begin
-- -- 			arc_pcb_contour_properties (cursor, log_threshold);
-- -- 		end;
-- -- 
-- -- 		procedure circle (cursor : in pac_pcb_contour_circles.cursor) is begin
-- -- 			circle_pcb_contour_properties (cursor, log_threshold);
-- -- 		end;
-- 		
-- 	begin -- log_plated_millings
-- 		null;
-- -- CS
-- -- 		iterate (millings.lines, line'access);
-- -- 		iterate (millings.arcs, arc'access);
-- -- 		iterate (millings.circles, circle'access);
-- 	end log_plated_millings;

	
	
	
	function to_string (appearance : in type_package_appearance) return string is begin
		return to_lower (type_package_appearance'image (appearance));
	end;

	function to_appearance (appearance : in string) return type_package_appearance is begin
		return type_package_appearance'value (appearance);
	end;
	
	
	
	function to_string (
		description : in pac_package_description.bounded_string;
		verbose		: in boolean := false) return string is
	begin
		if verbose then
			return "description '" & pac_package_description.to_string (description) & "'";
		else
			return pac_package_description.to_string (description);
		end if;
	end to_string;

	function to_package_description (description : in string) 
		return pac_package_description.bounded_string 
	is begin
		return pac_package_description.to_bounded_string (description);
	end to_package_description;

	function locate_package_model (model_name : in pac_package_model_file_name.bounded_string) -- ../lbr/smd/SO15.pac
		return pac_packages_lib.cursor 
	is begin
		return pac_packages_lib.find (packages_lib, model_name);
	end;

	
	function is_real (package_name : in pac_package_model_file_name.bounded_string) return boolean is
			use pac_packages_lib;
		cursor : pac_packages_lib.cursor;
	begin
		cursor := find (packages_lib, package_name);

		if element (cursor).appearance = REAL then
			return true;
		else
			return false;
		end if;
	end is_real;

	
	function terminal_properties (
		cursor		: in pac_packages_lib.cursor;
		terminal	: in pac_terminal_name.bounded_string) -- H4, 14
		return pac_terminals.cursor 
	is
		terminal_cursor : pac_terminals.cursor;

		procedure query_terminals (
			model_name	: in pac_package_model_file_name.bounded_string;
			model		: in type_package_lib) 
		is
			use pac_terminals;
		begin
			terminal_cursor := find (model.terminals, terminal);
		end;
		
	begin -- terminal_position
		pac_packages_lib.query_element (
			position	=> cursor,
			process		=> query_terminals'access);

		return terminal_cursor;
	end terminal_properties;
	

	function get_conductor_contours (
		package_cursor	: in pac_packages_lib.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_conductor_contours.list
	is
		result : pac_conductor_contours.list;
	begin
		-- CS
		return result;
	end get_conductor_contours;
	
	

	function to_string (flipped : in type_flipped) return string is begin
		return to_lower (type_flipped'image (flipped));
	end;

	function to_flipped (flipped : in string) return type_flipped is begin
		return type_flipped'value (flipped);
	end;

	


	procedure move_contours (
		term_pos	: in out type_position; -- terminal position
		outline		: in out type_contour'class;
		flipped		: in type_flipped;
		package_pos	: in type_package_position) 
	is 
		package_rotation : constant type_rotation := get_rotation (package_pos);
		package_position_relative : constant type_distance_relative := to_distance_relative (package_pos.place);
	begin
		---- Rotate the given terminal position by the position of the package:
		--rotate_by (term_pos, package_rotation);

		-- Rotate the given terminal position by the rotation of the package:
		rotate_by (term_pos.place, package_rotation);

		-- If the package is flipped, then the terminal position
		-- must be mirrored along the Y axis.
		if flipped = YES then mirror (term_pos.place, Y); end if;
		
		-- Move the given terminal position by the position of the package.
		move_by (term_pos.place, package_position_relative);
		-- The terminal position is now ready for drawing the terminal
		-- name and the pad outline.

		-- The terminal position will later be the offset by which the outline will be moved
		-- to its final place.

		
		if flipped = YES then
			-- The outline must be rotated by the rotation of the package
			-- minus the rotation of the given position itself:
			rotate_by (outline, add (package_rotation, - get_rotation (term_pos)));

			-- If the package is flipped, then the
			-- given outline (of a pad or a milled hole)
			-- must be mirrored along the Y axis.
			mirror (outline, Y); 
		else				
			-- The outline must be rotated by the rotation of the package
			-- plus the rotation of the given position itself:
			rotate_by (outline, add (package_rotation, get_rotation (term_pos)));
		end if;
		
		-- Move the outline to its final position:
		move_by (outline, to_distance_relative (term_pos.place));
	end move_contours;


	
	

	procedure placeholder_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_text_placeholders;
		placeholder : type_text_placeholder;
	begin
		placeholder := element (cursor);
		log (text => "silk screen placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), level => log_threshold);
		
		log_indentation_up;
		log (text => text_properties (type_text (placeholder)), level => log_threshold + 1);
		log_indentation_down;
	end placeholder_silk_screen_properties;
	


	


	procedure placeholder_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in type_log_level) is
		use pac_text_placeholders;
		placeholder : type_text_placeholder;
	begin
		placeholder := element (cursor);
		log (text => "assembly doc placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), level => log_threshold);

		log_indentation_up;
		log (text => text_properties (type_text (placeholder)), level => log_threshold + 1);
		log_indentation_down;
	end placeholder_assy_doc_properties;

	
	
	


	
-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	--procedure line_pcb_contour_properties (
		--cursor			: in pac_pcb_contour_lines.cursor;
		--log_threshold 	: in type_log_level)
	--is
		--use pac_pcb_contour_lines;
		--line : type_pcb_contour_line;
	--begin
		--line := element (cursor);
		--log (text => "PCB contour (edge cuts / outline) line" & latin_1.space
			 --& to_string (type_line (line)), level => log_threshold);
	--end line_pcb_contour_properties;

	--procedure arc_pcb_contour_properties (
		--cursor			: in pac_pcb_contour_arcs.cursor;
		--log_threshold 	: in type_log_level)
	--is
		--use pac_pcb_contour_arcs;
		--arc : type_pcb_contour_arc;
	--begin
		--arc := element (cursor);
		--log (text => "PCB contour (edge cuts / outline) arc" & latin_1.space 
			 --& to_string (type_arc (arc)), level => log_threshold);
	--end arc_pcb_contour_properties;

	--procedure circle_pcb_contour_properties (
		--cursor			: in pac_pcb_contour_circles.cursor;
		--log_threshold 	: in type_log_level)
	--is
		--use pac_pcb_contour_circles;
		--circle : type_pcb_contour_circle;
	--begin
		--circle := element (cursor);
		--log (text => "PCB contour (edge cuts / outline) circle" & latin_1.space 
			--& to_string (type_circle (circle)), level => log_threshold);
	--end circle_pcb_contour_properties;


	
	
	
	

-- 	procedure terminal_properties (
-- 	-- Logs the properties of the given terminal.
-- 		terminal		: in type_terminal;
-- 		name			: in pac_terminal_name.bounded_string;
-- 		log_threshold 	: in type_log_level) is
-- 		use et_pcb_coordinates;
-- 		log_threshold_1 : type_log_level := log_threshold + 1;
-- 
-- -- 		use type_pad_lines;
-- -- 		use type_pad_arcs;
-- -- 		use type_pad_circles;
-- -- 		use type_pad_polygons;		
-- 		
-- -- 		procedure line (cursor : in type_pad_lines.cursor) is begin
-- -- 			log (text => to_string (shapes.type_line (element (cursor))), level => log_threshold + 1);
-- -- 		end line;
-- -- 
-- -- 		procedure arc (cursor : in type_pad_arcs.cursor) is begin
-- -- 			log (text => to_string (shapes.type_arc (element (cursor))), level => log_threshold + 1);
-- -- 		end arc;
-- -- 		
-- -- 		procedure circle (cursor : in type_pad_circles.cursor) is begin
-- -- 			log (text => to_string (shapes.type_circle (element (cursor))), level => log_threshold + 1);
-- -- 		end circle;
-- -- 
-- -- 		procedure polygon (cursor : in type_pad_polygons.cursor) is 
-- -- 			use type_polygon_points;
-- -- 			points : type_polygon_points.set := element (cursor).corners;
-- -- 
-- -- 			procedure point (cursor : in type_polygon_points.cursor) is begin
-- -- 				log (text => to_string (element (cursor)), level => log_threshold + 1);	
-- -- 			end point;
-- -- 	
-- -- 		begin -- polygon
-- -- 			log (text => "polygon with corners", level => log_threshold + 1);
-- -- 			log_indentation_up;
-- -- 			iterate (points, point'access);
-- -- 			log_indentation_down;
-- -- 		end polygon;
-- 			
-- 	begin -- terminal_properties
-- 		log (text => "terminal name " & to_string (name)
-- 			& " technology" & to_string (terminal.technology)
-- 			& to_string (type_point (terminal.position))
-- 			& " rotation" & to_string (rot (terminal.position)),
-- 			level => log_threshold);
-- 
-- 		log_indentation_up;
-- 
-- 		case terminal.technology is
-- 			when THT => 
-- 				
-- 				-- log pad_shape_top/bottom
-- 				log (text => "pad contour top", level => log_threshold + 1);
-- -- 				iterate (terminal.pad_shape_tht.top.lines, line'access);
-- -- 				iterate (terminal.pad_shape_tht.top.arcs, arc'access);
-- -- 				iterate (terminal.pad_shape_tht.top.circles, circle'access);
-- -- 				iterate (terminal.pad_shape_tht.top.polygons, polygon'access);
-- 
-- 				log (text => "pad contour bottom", level => log_threshold + 1);
-- -- 				iterate (terminal.pad_shape_tht.bottom.lines, line'access);
-- -- 				iterate (terminal.pad_shape_tht.bottom.arcs, arc'access);
-- -- 				iterate (terminal.pad_shape_tht.bottom.circles, circle'access);
-- -- 				iterate (terminal.pad_shape_tht.bottom.polygons, polygon'access);
-- 				
-- 				log (text => "copper width of inner layers" & to_string (terminal.width_inner_layers), level => log_threshold_1);
-- 
-- 				case terminal.tht_hole is
-- 					when DRILLED =>
-- 						log (text => "drill" & to_string (terminal.drill_size), level => log_threshold_1); 
-- 					when MILLED =>
-- 						if log_level >= log_threshold_1 then
-- 							log (text => "plated milling contour ");
-- 							log_indentation_up;
-- 								log_plated_millings (terminal.millings, log_threshold_1);
-- 							log_indentation_down;
-- 						end if;
-- 				end case;
-- 				
-- 			when SMT => 
-- 				
-- 				-- log pad_shape
-- 				log (text => "pad contour", level => log_threshold + 1);
-- -- 				iterate (terminal.pad_shape.lines, line'access);
-- -- 				iterate (terminal.pad_shape.arcs, arc'access);
-- -- 				iterate (terminal.pad_shape.circles, circle'access);
-- -- 				iterate (terminal.pad_shape.polygons, polygon'access);
-- 				
-- 				log (text => "face" & to_string (terminal.face), level => log_threshold_1);
-- 				log (text => "stop mask" & to_string (terminal.stop_mask), level => log_threshold_1);
-- 				log (text => "solder paste" & to_string (terminal.solder_paste), level => log_threshold_1);
-- 		end case;
-- 
-- 		log_indentation_down;
-- 	end terminal_properties;


	



	
end et_packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
