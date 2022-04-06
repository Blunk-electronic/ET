------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       FILL ZONES IN BOARDS                               --
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
--   to do:

with ada.strings.unbounded;
with ada.tags;

package body et_fill_zones.boards is

	function to_string (priority_level : in type_fill_zone_priority) return string is begin
		return type_fill_zone_priority'image (priority_level);
	end;

	function to_fill_zone_priority (priority_level : in string) return type_fill_zone_priority is begin
		return type_fill_zone_priority'value (priority_level);
	end;




	
	function to_string (fill_zone_pad_connection : in type_fill_zone_pad_connection) return string is begin
		return to_lower (type_fill_zone_pad_connection'image (fill_zone_pad_connection));
	end;

	function to_pad_connection (connection : in string) return type_fill_zone_pad_connection is begin
		return type_fill_zone_pad_connection'value (connection);
	end;

	
	function to_string (fill_zone_pad_technology : in type_fill_zone_pad_technology) return string is begin
		return to_lower (type_fill_zone_pad_technology'image (fill_zone_pad_technology));
	end;

	function to_pad_technology (technology : in string) return type_fill_zone_pad_technology is begin
		return type_fill_zone_pad_technology'value (technology);
	end to_pad_technology;

	
	
	function conductor_fill_zone_properties_to_string (
		fill_zone		: in type_zone'class;
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

		
		procedure connected_with_net (p : in type_route_solid) is begin
			case p.connection is
				when THERMAL => NULL;

				when SOLID => null;
			end case;

		end connected_with_net;

		
	begin -- conductor_fill_zone_properties_to_string

		if fill_zone'tag = type_floating_solid'tag 
		or fill_zone'tag = type_floating_hatched'tag 
		then
			append ("floating");
			
		elsif fill_zone'tag = type_route_solid'tag 
		or    fill_zone'tag = type_hatched_route'tag 
		then
			append ("net " & pac_net_name.to_string (net_name));
			
			-- CS connected_with_net (type_route_solid (fill_zone));
		end if;

		
		case fill_zone.fill_style is
			when SOLID =>
				append (keyword_fill_style & space & to_string (fill_zone.fill_style));

				
			when HATCHED =>
				append (keyword_fill_style & space & to_string (fill_zone.fill_style));

		end case;

		append (keyword_min_width & to_string (fill_zone.width_min));
		append (keyword_isolation & to_string (fill_zone.isolation));
		append (keyword_easing_style & space & to_string (fill_zone.easing.style));
		append (keyword_easing_radius & to_string (fill_zone.easing.radius));
		
		append (keyword_layer & space & to_string (properties.layer));
		append (keyword_priority & to_string (properties.priority_level));

		return to_string (result);

	end conductor_fill_zone_properties_to_string;

	
end et_fill_zones.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
