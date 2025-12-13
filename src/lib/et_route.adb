------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              ROUTE                                       --
--                                                                          --
--                             B o d y                                      --
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
--   to do:
--




package body et_route is
	

	function get_polygons (
		route 			: in type_net_route;
		layer_category 	: in type_signal_layer_category;
		layer			: in type_signal_layer;
		bottom_layer	: in type_signal_layer)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;


		-- This procedure queries a conductor line, converts it
		-- to a polygon and appends it to the result:
		procedure query_line (l : in pac_conductor_lines.cursor) is
			use pac_conductor_lines;
			line : type_conductor_line renames element (l);
		begin
			if line.layer = layer then
				-- log (text => "line: " & to_string (line), level => log_threshold + 2);
				result.append (to_polygon (line, fill_tolerance));
			end if;
		end query_line;

		
		
		-- This procedure queries a conductor arc, converts it
		-- to a polygon and appends it to the result:				
		procedure query_arc (a : in pac_conductor_arcs.cursor) is
			use pac_conductor_arcs;
			arc : type_conductor_arc renames element (a);
		begin
			if arc.layer = layer then
				-- log (text => "arc: " & to_string (arc), level => log_threshold + 2);
				result.append (to_polygon (arc, fill_tolerance));
			end if;
		end query_arc;


				
		-- This procedure queries a via, converts it
		-- to a polygon and appends it to the result:
		procedure query_via (v : in pac_vias.cursor) is
			use pac_vias;
			via : type_via renames element (v);
		begin
			-- CS use function via_to_polyon 
			case via.category is
				when THROUGH =>
					if layer_category = OUTER_TOP or layer_category = OUTER_BOTTOM then
						result.append (to_polygon (via.position, via.restring_outer, via.diameter, fill_tolerance));
					else
						result.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
					end if;

				when BLIND_DRILLED_FROM_TOP =>
					if layer_category = OUTER_TOP then
						result.append (to_polygon (via.position, via.restring_top, via.diameter, fill_tolerance));
					elsif blind_via_uses_layer (via, layer, bottom_layer) then
						result.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
					end if;

				when BLIND_DRILLED_FROM_BOTTOM =>
					if layer_category = OUTER_BOTTOM then
						result.append (to_polygon (via.position, via.restring_bottom, via.diameter, fill_tolerance));
					elsif blind_via_uses_layer (via, layer, bottom_layer) then
						result.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
					end if;
					
				when BURIED =>
					if layer_category = INNER and then
					buried_via_uses_layer (via, layer) then
						result.append (to_polygon (via.position, via.restring_inner, via.diameter, fill_tolerance));
					end if;
			end case;
		end query_via;

		
	begin
		-- Iterate the lines, arcs and vias of the given route:
		route.lines.iterate (query_line'access);
		route.arcs.iterate (query_arc'access);
		route.vias.iterate (query_via'access);
		
		return result;
	end get_polygons;



	 
	
end et_route;



-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
