------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             PACKAGES                                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 -2024                                                 --
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






	procedure validate_pad_size (size : in type_distance_model) is
	begin
		if size not in type_pad_size then
			log (ERROR, "pad size invalid ! Allowed range is" 
				 & to_string (type_pad_size'first) & " .."
				 & to_string (type_pad_size'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_pad_size;
	
-- 	procedure validate_track_clearance (clearance : in et_pcb_coordinates.type_distance_model) is
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

-- 	procedure validate_restring_width (restring_width : in et_pcb_coordinates.type_distance_model) is
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


	procedure mirror_conductor_objects (
		conductors	: in out type_conductor_objects;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is begin
		mirror_lines (conductors.lines, axis);
		mirror_arcs (conductors.arcs, axis);
		mirror_circles (conductors.circles, axis);
		mirror_texts (conductors.texts);
	end mirror_conductor_objects;


	procedure rotate_conductor_objects (
		conductors	: in out type_conductor_objects;
		angle		: in type_rotation_model)
	is begin
		rotate_lines (conductors.lines, angle);
		rotate_arcs (conductors.arcs, angle);
		rotate_circles (conductors.circles, angle);
		rotate_texts (conductors.texts, angle);
	end rotate_conductor_objects;

	

	procedure move_conductor_objects (
		conductors	: in out type_conductor_objects;
		offset		: in type_distance_relative)
	is begin
		move_lines (conductors.lines, offset);
		move_arcs (conductors.arcs, offset);
		move_circles (conductors.circles, offset);
		move_texts (conductors.texts, offset);
	end move_conductor_objects;

	
	function to_polygons (
		conductors	: in type_conductor_objects;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result, scratch : pac_polygon_list.list;
	begin
		-- lines:
		result := to_polygons (conductors.lines, tolerance);

		-- arcs:
		scratch := to_polygons (conductors.arcs, tolerance);
		result.splice (before => pac_polygon_list.no_element, source => scratch);

		-- circles (outer edges only ):
		scratch := to_polygons_outside (conductors.circles, tolerance);
		result.splice (before => pac_polygon_list.no_element, source => scratch);
		
		-- texts
		scratch := to_polygons (conductors.texts);
		result.splice (before => pac_polygon_list.no_element, source => scratch);
		
		return result;
	end to_polygons;


	
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

	
	function get_package_model (model_name : in pac_package_model_file_name.bounded_string) -- ../lbr/smd/SO15.pac
		return pac_package_models.cursor 
	is begin
		return pac_package_models.find (package_models, model_name);
	end;

	
	function is_real (package_name : in pac_package_model_file_name.bounded_string) return boolean is
			use pac_package_models;
		cursor : pac_package_models.cursor;
	begin
		cursor := find (package_models, package_name);

		if element (cursor).appearance = REAL then
			return true;
		else
			return false;
		end if;
	end is_real;

	
	function get_terminal (
		cursor		: in pac_package_models.cursor;
		terminal	: in pac_terminal_name.bounded_string) -- H4, 14
		return pac_terminals.cursor 
	is
		terminal_cursor : pac_terminals.cursor;

		procedure query_terminals (
			model_name	: in pac_package_model_file_name.bounded_string;
			model		: in type_package_model) 
		is
			use pac_terminals;
		begin
			terminal_cursor := find (model.terminals, terminal);
		end;
		
	begin
		pac_package_models.query_element (
			position	=> cursor,
			process		=> query_terminals'access);

		return terminal_cursor;
	end get_terminal;
	

	function get_terminal_contours (
		package_cursor	: in pac_package_models.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_contour_list.list
	is
		packge : type_package_model renames element (package_cursor);
		
		result : pac_contour_list.list;
		

		procedure query_terminal (t : in pac_terminals.cursor) is
			use pac_terminals;
			terminal : type_terminal renames element (t);

			displacement : constant type_distance_relative :=
				to_distance_relative (terminal.position.place);

			procedure finalize (c : in type_contour) is 
				contour : type_contour := c;
			begin
				rotate_by (contour, terminal.position.rotation);
				move_by (contour, displacement);
				result.append (contour);
			end finalize;
	
		begin
			case terminal.technology is
				when THT => 
					case layer_category is
						when INNER =>								
							case terminal.tht_hole is
								when DRILLED =>
									finalize (get_inner_contour (
										terminal, to_vector (terminal.position.place)));

								when MILLED =>
									finalize (terminal.millings);
							end case;

							
						when OUTER_TOP =>
							finalize (terminal.pad_shape_tht.top);
							
						when OUTER_BOTTOM =>
							finalize (terminal.pad_shape_tht.bottom);
					end case;
					

				when SMT =>
					case layer_category is
						when OUTER_TOP =>
							if terminal.face = TOP then
								finalize (terminal.pad_shape_smt);
							end if;
						
						when OUTER_BOTTOM =>
							if terminal.face = BOTTOM then
								finalize (terminal.pad_shape_smt);
							end if;

						when INNER => null; -- there are no SMT pads in inner layers
					end case;
			end case;
		end query_terminal;
		
	begin
		packge.terminals.iterate (query_terminal'access);
		return result;
	end get_terminal_contours;


	function get_conductor_objects (
		package_cursor	: in pac_package_models.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects
	is 
		packge : type_package_model renames element (package_cursor);
	begin
		case layer_category is
			when OUTER_TOP =>
				return packge.conductors.top;

			when OUTER_BOTTOM =>
				return packge.conductors.bottom;

			when INNER =>
				raise constraint_error; -- CS semantic_error ? Error message ?
		end case;
	end get_conductor_objects;
	

	function get_route_restrict_objects (
		package_cursor	: in pac_package_models.cursor;
		layer_category	: in type_signal_layer_category)
		return et_route_restrict.packages.type_one_side
	is
		packge : type_package_model renames element (package_cursor);
	begin
		case layer_category is
			when OUTER_TOP =>
				return packge.route_restrict.top;

			when OUTER_BOTTOM =>
				return packge.route_restrict.bottom;

			when INNER =>
				raise constraint_error; -- CS semantic_error ? Error message ?
		end case;
	end get_route_restrict_objects;


	function get_via_restrict_objects (
		package_cursor	: in pac_package_models.cursor;
		layer_category	: in type_signal_layer_category)
		return et_via_restrict.packages.type_one_side
	is
		packge : type_package_model renames element (package_cursor);
	begin
		case layer_category is
			when OUTER_TOP =>
				return packge.via_restrict.top;

			when OUTER_BOTTOM =>
				return packge.via_restrict.bottom;

			when INNER =>
				raise constraint_error; -- CS semantic_error ? Error message ?
		end case;
	end get_via_restrict_objects;

	
	function get_hole_contours (
		package_cursor	: in pac_package_models.cursor)
		return pac_holes.list
	is 
		packge : type_package_model renames element (package_cursor);
	begin
		return packge.holes;
	end get_hole_contours;


	function get_keepout_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return type_keepout
	is
		packge : type_package_model renames element (package_cursor);
	begin
		case face is
			when TOP	=> return packge.keepout.top;
			when BOTTOM	=> return packge.keepout.bottom;
		end case;
	end get_keepout_objects;
	

	function get_stencil_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return et_stencil.type_stencil
	is
		packge : type_package_model renames element (package_cursor);
	begin
		case face is
			when TOP	=> return packge.stencil.top;
			when BOTTOM	=> return packge.stencil.bottom;
		end case;
	end get_stencil_objects;


	function get_stopmask_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return et_stop_mask.type_stopmask
	is
		packge : type_package_model renames element (package_cursor);
	begin
		case face is
			when TOP	=> return packge.stop_mask.top;
			when BOTTOM	=> return packge.stop_mask.bottom;
		end case;
	end get_stopmask_objects;
	


	function get_silkscreen_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return type_silkscreen_package
	is
		packge : type_package_model renames element (package_cursor);
	begin
		case face is
			when TOP	=> return packge.silkscreen.top;
			when BOTTOM	=> return packge.silkscreen.bottom;
		end case;
	end get_silkscreen_objects;


	function get_assy_doc_objects (
		package_cursor	: in pac_package_models.cursor;
		face			: in type_face)
		return type_assy_doc_package
	is
		packge : type_package_model renames element (package_cursor);
	begin
		case face is
			when TOP	=> return packge.assy_doc.top;
			when BOTTOM	=> return packge.assy_doc.bottom;
		end case;
	end get_assy_doc_objects;


	
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
		package_rotation : constant type_rotation_model := get_rotation (package_pos);
		package_position_relative : constant type_distance_relative := to_distance_relative (package_pos.place);
	begin
		---- Rotate the given terminal position by the position of the package:
		--rotate_by (term_pos, package_rotation);

		-- Rotate the given terminal position by the rotation of the package:
		rotate_by (term_pos.place, package_rotation);

		-- If the package is flipped, then the terminal position
		-- must be mirrored along the Y axis.
		if flipped = YES then mirror (term_pos.place, MIRROR_ALONG_Y_AXIS); end if;
		
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
			mirror (outline, MIRROR_ALONG_Y_AXIS); 
		else				
			-- The outline must be rotated by the rotation of the package
			-- plus the rotation of the given position itself:
			rotate_by (outline, add (package_rotation, get_rotation (term_pos)));
		end if;
		
		-- Move the outline to its final position:
		move_by (outline, to_distance_relative (term_pos.place));
	end move_contours;


	
	

	
	
	


	
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
