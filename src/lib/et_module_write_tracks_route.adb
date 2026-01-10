------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / BOARD TRACKS ROUTE                     --
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
--                                                                          --
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
-- ToDo:
-- - clean up
-- - move write via to separate package
-- - move write fill zone to separate package
--

with ada.text_io;					use ada.text_io;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_section_headers;			use et_section_headers;
with et_pcb_stack;					use et_pcb_stack;
with et_route;						use et_route;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_board_geometry;				use et_board_geometry;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_primitive_objects;			use et_primitive_objects;
with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_vias;						use et_vias;
with et_drills;						use et_drills;
with et_terminals;					use et_terminals;
with et_thermal_relief;				use et_thermal_relief;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;

with et_general_rw;					use et_general_rw;
with et_board_write;				use et_board_write;

with et_module_read_nets;



package body et_module_write_tracks_route is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_signal_layers;



	
	procedure query_route (
		net_name	: in pac_net_name.bounded_string;
		net			: in type_net) 
	is
		use et_board_geometry;
		use pac_contours;

		use et_board_geometry.pac_geometry_2;
		
		use et_conductor_segment.boards;

		use et_fill_zones;
		use et_fill_zones.boards;
		use et_thermal_relief;		

		use boards.pac_cutouts;
		


		procedure write_lines is
			use pac_conductor_lines;
			line_cursor : pac_conductor_lines.cursor := net.route.lines.first;
		begin
			while line_cursor /= pac_conductor_lines.no_element loop
				section_mark (section_line, HEADER);
				
				write (keyword => keyword_start, parameters => to_string (get_A (line_cursor), FORMAT_2));
				write (keyword => keyword_end  , parameters => to_string (get_B (line_cursor), FORMAT_2));
				write (keyword => keyword_layer, parameters => to_string (element (line_cursor).layer));
				write (keyword => keyword_width, parameters => to_string (element (line_cursor).width));
				-- CS functions required get_A (line_cursor) return string
				-- also for layer, width, center, ...

				section_mark (section_line, FOOTER);
				next (line_cursor);
			end loop;
		end write_lines;



		


		procedure write_arcs is
			use pac_conductor_arcs;
			arc_cursor : pac_conductor_arcs.cursor := net.route.arcs.first;
		begin		
			while arc_cursor /= pac_conductor_arcs.no_element loop
				section_mark (section_arc, HEADER);

				write (keyword => keyword_center, parameters => to_string (get_center (element (arc_cursor)), FORMAT_2));
				write (keyword => keyword_start , parameters => to_string (get_A (arc_cursor), FORMAT_2));
				write (keyword => keyword_end   , parameters => to_string (get_B (arc_cursor), FORMAT_2));
				write (keyword => keyword_width , parameters => to_string (element (arc_cursor).width));
				write (keyword => keyword_layer , parameters => to_string (element (arc_cursor).layer));
				
				section_mark (section_arc, FOOTER);
				next (arc_cursor);
			end loop;
		end write_arcs;

		




		
		procedure write_zones_solid is
			use pac_route_solid; 
			polygon_solid_cursor : pac_route_solid.cursor := net.route.zones.solid.first;

			
			procedure query_zone (zone : in type_route_solid) is
			begin
				fill_zone_begin;


				write (keyword => keyword_easing_style,
					parameters => to_string (zone.easing.style));
				
				write (keyword => keyword_easing_radius, 
					parameters => to_string (zone.easing.radius));

				
				
				write_width (zone.linewidth);

				write (keyword => keyword_isolation, 
				   parameters => to_string (zone.isolation));
				
				write (keyword => keyword_priority , 
					   parameters => to_string (zone.properties.priority_level));
				
				write (keyword => keyword_layer, parameters => to_string (zone.properties.layer));

				write (keyword => keyword_fill_style, 
					parameters => to_string (et_primitive_objects.SOLID));

					
				case zone.connection is
					when THERMAL => 
						write (keyword => keyword_connection, 
							parameters => to_string (zone.connection));
						
						write (keyword => keyword_pad_technology,
							parameters => to_string (zone.relief_properties.technology));
						
						write (keyword => keyword_relief_width_min, 
							parameters => to_string (zone.relief_properties.width_min));
						
						write (keyword => keyword_relief_gap_max,
							parameters => to_string (zone.relief_properties.gap_max));	

						
						
					when SOLID =>
						write (keyword => keyword_pad_technology, parameters => to_string (zone.technology));
						
				end case;

				contours_begin;
				write_polygon_segments (type_contour (zone));
				contours_end;
				
				fill_zone_end;
			end query_zone;

			
		begin
			while polygon_solid_cursor /= pac_route_solid.no_element loop
				query_element (polygon_solid_cursor, query_zone'access);				
				next (polygon_solid_cursor);
			end loop;
		end write_zones_solid;





		

		procedure write_zones_hatched is
			use pac_route_hatched;
			polygon_hatched_cursor : pac_route_hatched.cursor := net.route.zones.hatched.first;

			
			procedure query_zone (zone : in type_route_hatched) is
			begin
				fill_zone_begin;


				write (keyword => keyword_easing_style,
					parameters => to_string (zone.easing.style));
				
				write (keyword => keyword_easing_radius, 
					parameters => to_string (zone.easing.radius));

				

				write_width (zone.linewidth);

				write (keyword => keyword_isolation, 
				   parameters => to_string (zone.isolation));
				
				write (keyword => keyword_priority , 
					parameters => to_string (zone.properties.priority_level));
				
				write (keyword => keyword_layer, 
					parameters => to_string (zone.properties.layer));

				write (keyword => keyword_fill_style, 
					parameters => to_string (HATCHED));

				
				write (keyword => keyword_spacing, 
					parameters => to_string (zone.spacing));

				case zone.connection is
					when THERMAL => 
						write (keyword => keyword_connection, 
							parameters => to_string (zone.connection));
						
						write (keyword => keyword_pad_technology,
							parameters => to_string (zone.relief_properties.technology));
						
						write (keyword => keyword_relief_width_min, 
							parameters => to_string (zone.relief_properties.width_min));
						
						write (keyword => keyword_relief_gap_max,
							parameters => to_string (zone.relief_properties.gap_max));	

		
					when SOLID =>
						write (keyword => keyword_pad_technology, parameters => to_string (zone.technology));

				end case;

				contours_begin;
				write_polygon_segments (type_contour (zone));
				contours_end;
				
				fill_zone_end;
			end query_zone;

			
		begin
			while polygon_hatched_cursor /= pac_route_hatched.no_element loop
				query_element (polygon_hatched_cursor, query_zone'access);
				next (polygon_hatched_cursor);
			end loop;
		end write_zones_hatched;
		




		
		
		procedure write_vias is
			use et_vias;
			use pac_vias;

			
			procedure query_via (c : in pac_vias.cursor) is begin
				section_mark (section_via, HEADER);

				write (keyword => keyword_via_category, parameters => to_string (element (c).category));
				write (keyword => keyword_position, parameters => to_string (element (c).position, FORMAT_2));
				write (keyword => keyword_diameter, parameters => to_string (element (c).diameter));
				-- CS function get_position (c) return string
				-- CS also for category and diameter

				case element (c).category is
					when THROUGH =>
						write (keyword => keyword_restring_outer,
							parameters => to_string (element (c).restring_outer));
						
					when BLIND_DRILLED_FROM_TOP =>
						write (keyword => keyword_restring_outer, parameters => to_string (element (c).restring_top));
						write (keyword => keyword_destination, parameters => to_string (element (c).lower));
						
					when BLIND_DRILLED_FROM_BOTTOM =>
						write (keyword => keyword_restring_outer, parameters => to_string (element (c).restring_bottom));
						write (keyword => keyword_destination, parameters => to_string (element (c).upper));
						
					when BURIED =>
						write (keyword => keyword_layers, parameters => to_string (element (c).layers));
				end case;

				write (keyword => keyword_restring_inner,
					parameters => to_string (element (c).restring_inner));
				
				section_mark (section_via, FOOTER);
			end query_via;

			
		begin
			net.route.vias.iterate (query_via'access);
		end write_vias;


		
		
	begin
		section_mark (section_route, HEADER);

		write_lines;
		write_arcs;
		write_vias;

		write_zones_solid;
		write_zones_hatched;

		
		
		-- cutout zones -> now net specific restrict areas
		-- CS
		--while cutout_zone_cursor /= pac_cutouts.no_element loop
			--cutout_zone_begin;
			--write_signal_layer (element (cutout_zone_cursor).layer);

			--contours_begin;
			--write_polygon_segments (type_contour (element (cutout_zone_cursor)));
			--contours_end;
			
			--cutout_zone_end;
			--next (cutout_zone_cursor);
		--end loop;
		
		section_mark (section_route, FOOTER);
	end query_route;


end et_module_write_tracks_route;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
