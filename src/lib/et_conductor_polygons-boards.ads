------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--            POLYGONS IN CONDUCTOR LAYERS OF BOARDS (PCB)                  --
--                                                                          --
--                               S p e c                                    --
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


package et_conductor_polygons.boards is

	-- All fill zones in conductor layers have these common
	-- properties:
	type type_conductor_polygon_properties is record
		layer 			: type_signal_layer := type_signal_layer'first;
		priority_level	: type_polygon_priority := type_polygon_priority'first;
		fill_lines		: pac_fill_lines.list;
	end record;

	
	function conductor_polygon_properties_to_string (
		polygon			: in type_polygon_conductor'class;
		properties		: in type_conductor_polygon_properties;

		-- Net name is relevant if polygon is part of a route.
		-- The type of the given polygon is the cirteria:
		net_name		: in pac_net_name.bounded_string := no_name)
		return string;

	
	
	-- A floating conductor polygon is not connected to any net:
	type type_polygon_conductor_solid_floating is new 
		type_polygon_conductor (fill_style => SOLID)
	with record
		properties	: type_conductor_polygon_properties;
	end record;

	package pac_conductor_polygons_floating_solid is new 
		indefinite_doubly_linked_lists (type_polygon_conductor_solid_floating);

		
		
	type type_polygon_conductor_hatched_floating is new 
		type_polygon_conductor (fill_style => HATCHED) 
	with record
		properties	: type_conductor_polygon_properties;
	end record;

	package pac_conductor_polygons_floating_hatched is new
		indefinite_doubly_linked_lists (type_polygon_conductor_hatched_floating);

		
	type type_conductor_polygons_floating is record
		solid	: pac_conductor_polygons_floating_solid.list;
		hatched	: pac_conductor_polygons_floating_hatched.list;
	end record;



	type type_polygon_conductor_route_solid (connection : type_polygon_pad_connection) 
		is new type_polygon_conductor_solid
	with record
		properties	: type_conductor_polygon_properties;

		case connection is
			when THERMAL =>
				thermal : type_thermal;

			when SOLID =>
				-- whether SMT, THT or both kinds of pads connect with the polygon
				technology	: type_polygon_pad_technology;
				-- no need for any kind of thermal parameters
		end case;				
	end record;

	type type_polygon_conductor_route_hatched (connection : type_polygon_pad_connection) 
		is new type_polygon_conductor_hatched 
	with record
		properties	: type_conductor_polygon_properties;
				
		case connection is
			when THERMAL =>
				thermal : type_thermal;

			when SOLID =>
				-- whether SMT, THT or both kinds of pads connect with the polygon
				technology	: type_polygon_pad_technology;
				-- no need for any kind of thermal parameters
		end case;
	end record;


	
	package pac_signal_polygons_solid is new
		indefinite_doubly_linked_lists (type_polygon_conductor_route_solid);
	
	package pac_signal_polygons_hatched is new
		indefinite_doubly_linked_lists (type_polygon_conductor_route_hatched);	


		
	type type_signal_polygons is record
		solid	: pac_signal_polygons_solid.list;
		hatched	: pac_signal_polygons_hatched.list;
	end record;




	
	
	type type_conductor_cutout 
		is new type_polygon with
	record
		layer 	: type_signal_layer := type_signal_layer'first;
	end record;

	package pac_conductor_cutouts is new doubly_linked_lists (type_conductor_cutout);

	
end et_conductor_polygons.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
