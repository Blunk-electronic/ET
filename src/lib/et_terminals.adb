------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             TERMINALS                                    --
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

with et_exceptions;				use et_exceptions;

package body et_terminals is

	procedure validate_pad_size (size : in type_distance) is begin
		if size not in type_pad_size then
			raise semantic_error_1 with
				"ERROR: Pad size invalid ! Allowed range is" 
				 & to_string (type_pad_size'first) & " .."
				 & to_string (type_pad_size'last);
		end if;
	end validate_pad_size;


	
	procedure log_plated_millings (
		millings 		: in type_contour;
		log_threshold	: in type_log_level)
		is
-- 		use type_pcb_contour_lines;
-- 		use type_pcb_contour_arcs;
-- 		use type_pcb_contour_circles;
-- 		
-- 		procedure line (cursor : in type_pcb_contour_lines.cursor) is begin
-- 			line_pcb_contour_properties (cursor, log_threshold);
-- 		end;
-- 
-- 		procedure arc (cursor : in type_pcb_contour_arcs.cursor) is begin
-- 			arc_pcb_contour_properties (cursor, log_threshold);
-- 		end;
-- 
-- 		procedure circle (cursor : in type_pcb_contour_circles.cursor) is begin
-- 			circle_pcb_contour_properties (cursor, log_threshold);
-- 		end;
		
	begin -- log_plated_millings
		null;
-- CS
-- 		iterate (millings.lines, line'access);
-- 		iterate (millings.arcs, arc'access);
-- 		iterate (millings.circles, circle'access);
	end log_plated_millings;

	
	function to_string (solder_paste : in type_solder_paste_status) return string is begin
		return to_lower (type_solder_paste_status'image (solder_paste));
	end;

	function to_solder_paste_status (solder_paste : in string) return type_solder_paste_status is begin
		return type_solder_paste_status'value (solder_paste);
	end;
	
	function to_string (stop_mask : in type_stop_mask_status) return string is begin
		return to_lower (type_stop_mask_status'image (stop_mask));
	end;

	function to_stop_mask_status (stop_mask : in string) return type_stop_mask_status is begin
		return type_stop_mask_status'value (stop_mask);
	end;

	function to_string (shape : in type_stop_mask_shape) return string is begin
		return to_lower (type_stop_mask_shape'image (shape));
	end;

	function to_shape (shape : in string) return type_stop_mask_shape is begin
		return type_stop_mask_shape'value (shape);
	end;

	function to_string (shape : in type_stencil_shape) return string is begin
		return to_lower (type_stencil_shape'image (shape));
	end;

	function to_shape (shape : in string) return type_stencil_shape is begin
		return type_stencil_shape'value (shape);
	end;	
	
	function to_string (tht_hole : in type_terminal_tht_hole) return string is begin
		return to_lower (type_terminal_tht_hole'image (tht_hole));
	end;

	function to_tht_hole (tht_hole : in string) return type_terminal_tht_hole is begin
		return type_terminal_tht_hole'value (tht_hole);
	end;

	function to_string (technology : in type_assembly_technology) return string is begin
		return type_assembly_technology'image (technology);
	end;

	function to_assembly_technology (technology : in string) return type_assembly_technology is begin
		return type_assembly_technology'value (technology);
	end;
	
	function to_string (terminal : in pac_terminal_name.bounded_string) return string is begin
		return pac_terminal_name.to_string (terminal);
	end;

	function to_terminal_name (terminal : in string) return pac_terminal_name.bounded_string is begin
		return pac_terminal_name.to_bounded_string (terminal);
	end;

	
	procedure terminal_properties (
		terminal		: in type_terminal;
		name			: in pac_terminal_name.bounded_string;
		log_threshold 	: in type_log_level)
	is
		use et_pcb_coordinates;
		log_threshold_1 : type_log_level := log_threshold + 1;

-- 		use type_pad_lines;
-- 		use type_pad_arcs;
-- 		use type_pad_circles;
-- 		use type_pad_polygons;		
		
-- 		procedure line (cursor : in type_pad_lines.cursor) is begin
-- 			log (text => to_string (shapes.type_line (element (cursor))), level => log_threshold + 1);
-- 		end line;
-- 
-- 		procedure arc (cursor : in type_pad_arcs.cursor) is begin
-- 			log (text => to_string (shapes.type_arc (element (cursor))), level => log_threshold + 1);
-- 		end arc;
-- 		
-- 		procedure circle (cursor : in type_pad_circles.cursor) is begin
-- 			log (text => to_string (shapes.type_circle (element (cursor))), level => log_threshold + 1);
-- 		end circle;
-- 
-- 		procedure polygon (cursor : in type_pad_polygons.cursor) is 
-- 			use type_polygon_points;
-- 			points : type_polygon_points.set := element (cursor).corners;
-- 
-- 			procedure point (cursor : in type_polygon_points.cursor) is begin
-- 				log (text => to_string (element (cursor)), level => log_threshold + 1);	
-- 			end point;
-- 	
-- 		begin -- polygon
-- 			log (text => "polygon with corners", level => log_threshold + 1);
-- 			log_indentation_up;
-- 			iterate (points, point'access);
-- 			log_indentation_down;
-- 		end polygon;
			
	begin -- terminal_properties
		log (text => "terminal name " & to_string (name)
			& " technology" & to_string (terminal.technology)
			& to_string (terminal.position.place)
			& " rotation" & to_string (get_rotation (terminal.position)),
			level => log_threshold);

		log_indentation_up;

		case terminal.technology is
			when THT => 
				
				-- log pad_shape_top/bottom
				log (text => "pad contour top", level => log_threshold + 1);
-- 				iterate (terminal.pad_shape_tht.top.lines, line'access);
-- 				iterate (terminal.pad_shape_tht.top.arcs, arc'access);
-- 				iterate (terminal.pad_shape_tht.top.circles, circle'access);
-- 				iterate (terminal.pad_shape_tht.top.polygons, polygon'access);

				log (text => "pad contour bottom", level => log_threshold + 1);
-- 				iterate (terminal.pad_shape_tht.bottom.lines, line'access);
-- 				iterate (terminal.pad_shape_tht.bottom.arcs, arc'access);
-- 				iterate (terminal.pad_shape_tht.bottom.circles, circle'access);
-- 				iterate (terminal.pad_shape_tht.bottom.polygons, polygon'access);
				
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
-- 				iterate (terminal.pad_shape.lines, line'access);
-- 				iterate (terminal.pad_shape.arcs, arc'access);
-- 				iterate (terminal.pad_shape.circles, circle'access);
-- 				iterate (terminal.pad_shape.polygons, polygon'access);
				
				log (text => "face" & to_string (terminal.face), level => log_threshold_1);
				log (text => "stop mask status" & to_string (terminal.stop_mask_status_smt), level => log_threshold_1);
				log (text => "solder paste status" & to_string (terminal.solder_paste_status), level => log_threshold_1);
		end case;

		log_indentation_down;
	end terminal_properties;


	procedure iterate (
		terminals	: in pac_terminals.map;
		process		: not null access procedure (position : pac_terminals.cursor);
		proceed		: not null access boolean)
	is
		use pac_terminals;
		c : pac_terminals.cursor := terminals.first;
	begin
		while c /= pac_terminals.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
	
	
end et_terminals;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
