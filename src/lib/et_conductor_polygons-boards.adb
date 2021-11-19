------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--            POLYGONS IN CONDUCTOR LAYERS OF BOARDS (PCB)                  --
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

with ada.strings.unbounded;
with ada.tags;

package body et_conductor_polygons.boards is

	function to_string (priority_level : in type_polygon_priority) return string is begin
		return type_polygon_priority'image (priority_level);
	end;

	function to_polygon_priority (priority_level : in string) return type_polygon_priority is begin
		return type_polygon_priority'value (priority_level);
	end;




	
	function to_string (polygon_pad_connection : in type_polygon_pad_connection) return string is begin
		return to_lower (type_polygon_pad_connection'image (polygon_pad_connection));
	end;

	function to_pad_connection (connection : in string) return type_polygon_pad_connection is begin
		return type_polygon_pad_connection'value (connection);
	end;

	
	function to_string (polygon_pad_technology : in type_polygon_pad_technology) return string is begin
		return to_lower (type_polygon_pad_technology'image (polygon_pad_technology));
	end;

	function to_pad_technology (technology : in string) return type_polygon_pad_technology is begin
		return type_polygon_pad_technology'value (technology);
	end to_pad_technology;

	
	
	function conductor_polygon_properties_to_string (
		polygon			: in type_polygon_conductor'class;
		properties		: in type_properties;
		net_name		: in pac_net_name.bounded_string := no_name)
		return string
	is
		use ada.strings.unbounded;
		use ada.tags;
		--use et_nets;
		
		result : unbounded_string := to_unbounded_string ("properties:");

		procedure append (s : in string) is begin
			result := result & space & s;
		end append;

		procedure connected_with_net (p : in type_polygon_conductor_route_solid) is
		begin
			case p.connection is
				when THERMAL => NULL;

				when SOLID => null;
			end case;

		end connected_with_net;
		
	begin -- conductor_polygon_properties_to_string

		if polygon'tag = type_solid_floating'tag 
		or polygon'tag = type_hatched_floating'tag 
		then
			append ("floating");
			
		elsif polygon'tag = type_polygon_conductor_route_solid'tag 
		or    polygon'tag = type_polygon_conductor_route_hatched'tag 
		then
			append ("net " & pac_net_name.to_string (net_name));
			
			-- CS connected_with_net (type_polygon_conductor_route_solid (polygon));
		end if;

		
		case polygon.fill_style is
			when SOLID =>
				append (keyword_fill_style & space & to_string (polygon.fill_style));

				
			when HATCHED =>
				append (keyword_fill_style & space & to_string (polygon.fill_style));

		end case;

		append (keyword_min_width & to_string (polygon.width_min));
		append (keyword_isolation & to_string (polygon.isolation));
		append (keyword_easing_style & space & to_string (polygon.easing.style));
		append (keyword_easing_radius & to_string (polygon.easing.radius));
		
		append (keyword_layer & space & to_string (properties.layer));
		append (keyword_priority & to_string (properties.priority_level));

		return to_string (result);

	end conductor_polygon_properties_to_string;

	
end et_conductor_polygons.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
