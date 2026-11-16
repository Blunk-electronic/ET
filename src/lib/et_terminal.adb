------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         PACKAGE TERMINAL                                 --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;


package body et_terminal is


	function to_position (
		line : in type_fields_of_line; -- "x 23 y 0.2 rotation 90.0 face top"
		from : in type_field_count_positive)
		return type_terminal_position
	is
		result : type_terminal_position (THT);
	begin
		-- CS
		return result;
	end to_position;



	function to_string (
		position : in type_terminal_position_fine)
		return string
	is ("place: " & to_string (position.place)
			& " rotation: " & to_string (position.rotation)
			& " face: " & to_string (position.face)
			& " technology: " & to_string (position.technology));





	function get_inner_contour (
		terminal	: in type_terminal;
		position	: in type_vector)
		return type_contour
	is
		result : type_contour := (
			contour => (circular => true, circle => <>),
			others	=> <>);
	begin
		set_place (result.contour.circle, to_vector_model (position));
		set_radius (result.contour.circle, 0.5 * terminal.drill_size + terminal.width_inner_layers);

		return result;
	end get_inner_contour;





	procedure terminal_properties (
		terminal		: in type_terminal;
		name			: in type_terminal_name;
		log_threshold	: in type_log_level)
	is
		log_threshold_1 : constant type_log_level := log_threshold + 1;

--		use type_pad_lines;
--		use type_pad_arcs;
--		use type_pad_circles;
--		use type_pad_polygons;

--		procedure line (cursor : in type_pad_lines.cursor) is begin
--			log (text => to_string (shapes.type_line (element (cursor))), level => log_threshold + 1);
--		end line;
--
--		procedure arc (cursor : in type_pad_arcs.cursor) is begin
--			log (text => to_string (shapes.type_arc (element (cursor))), level => log_threshold + 1);
--		end arc;
--
--		procedure circle (cursor : in type_pad_circles.cursor) is begin
--			log (text => to_string (shapes.type_circle (element (cursor))), level => log_threshold + 1);
--		end circle;
--
--		procedure polygon (cursor : in type_pad_polygons.cursor) is
--			use type_polygon_points;
--			points : type_polygon_points.set := element (cursor).corners;
--
--			procedure point (cursor : in type_polygon_points.cursor) is begin
--				log (text => to_string (element (cursor)), level => log_threshold + 1);
--			end point;
--
--		begin -- polygon
--			log (text => "polygon with corners", level => log_threshold + 1);
--			log_indentation_up;
--			iterate (points, point'access);
--			log_indentation_down;
--		end polygon;

	begin -- terminal_properties
		log (text => "terminal name " & to_string (name)
			& " technology" & to_string (terminal.technology)
			& get_position (terminal),
			level => log_threshold);

		log_indentation_up;

		case terminal.technology is
			when THT =>

				-- log pad_shape_top/bottom
				log (text => "pad contour top", level => log_threshold + 1);
--				iterate (terminal.pad_shape_tht.top.lines, line'access);
--				iterate (terminal.pad_shape_tht.top.arcs, arc'access);
--				iterate (terminal.pad_shape_tht.top.circles, circle'access);
--				iterate (terminal.pad_shape_tht.top.polygons, polygon'access);

				log (text => "pad contour bottom", level => log_threshold + 1);
--				iterate (terminal.pad_shape_tht.bottom.lines, line'access);
--				iterate (terminal.pad_shape_tht.bottom.arcs, arc'access);
--				iterate (terminal.pad_shape_tht.bottom.circles, circle'access);
--				iterate (terminal.pad_shape_tht.bottom.polygons, polygon'access);

				log (text => "conductor width in inner layers" & to_string (terminal.width_inner_layers), level => log_threshold_1);

				case terminal.tht_hole is
					when DRILLED =>
						log (text => "drill" & to_string (terminal.drill_size), level => log_threshold_1);
					when MILLED =>
						if log_level >= log_threshold_1 then
							log (text => "plated milling contour ");
							log_indentation_up;
								log_plated_millings (terminal.millings, log_threshold_1);
							log_indentation_down;
						end if;
				end case;

			when SMT =>

				-- log pad_shape
				log (text => "pad contour", level => log_threshold + 1);
--				iterate (terminal.pad_shape.lines, line'access);
--				iterate (terminal.pad_shape.arcs, arc'access);
--				iterate (terminal.pad_shape.circles, circle'access);
--				iterate (terminal.pad_shape.polygons, polygon'access);

				log (text => "face" & to_string (terminal.face), level => log_threshold_1);
				log (text => "stop mask status" & to_string (terminal.stop_mask_status_smt), level => log_threshold_1);
				log (text => "solder paste status" & to_string (terminal.solder_paste_status), level => log_threshold_1);
		end case;

		log_indentation_down;
	end terminal_properties;



end et_terminal;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
