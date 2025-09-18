------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            NET CONNECTORS                                --
--                                                                          --
--                               S p e c                                    --
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


with cairo;

with et_fonts;

with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_schematic_text;					use et_schematic_text;
with et_object_status;					use et_object_status;
with et_string_processing;				use et_string_processing;


package et_net_connectors is

	use pac_geometry_2;
	use pac_text_schematic;
	
	
	type type_connector_direction is (
		-- CS POWER ?
		INPUT, 
		OUTPUT, 
		BIDIR, 
		TRISTATE, 
		PASSIVE); -- CS use prefix DIR_ ?
	
	connector_direction_default : constant type_connector_direction := PASSIVE;

	
	function to_string (direction : in type_connector_direction) return string;
	
	function to_direction (direction : in string) return type_connector_direction;


	

	
	-- A net connector can only be attached to a stub of a net, means to a 
	-- dead end of a net segment.
	-- The rotation of the connector depends on the direction of the stub.
	-- However, the shown text inside the connector (net name and coordinates) 
	-- is always readable from the front or from the right.
	
	type type_net_connector (active : boolean := false) is record
		case active is
			when TRUE =>
				size		: type_text_size := text_size_default;
				
				-- width		: et_schematic_text.type_text_line_width := et_schematic_text.type_text_line_width'first;
				-- CS probably no need ?
				
				status		: type_object_status;
				
				direction	: type_connector_direction := connector_direction_default;

			when FALSE => null;
		end case;
	end record;



	-- Converts a string like "direction input"
	-- to a net connector.
	-- If something is wrong, then the error-flag is set.
	-- This procedure modifies a connector. The connector
	-- must be set active by the caller beforehand,
	-- otherwise a discriminant exception will be raised:
	procedure make_net_connector (
		arguments	: in type_fields_of_line;
		error		: out boolean;
		connector	: out type_net_connector); -- must be active
	


	-- Returns something like "connector direction input":
	function to_string (
		connector : in type_net_connector)
		return string;
	

	function get_direction (
		connector	: in type_net_connector)
		return type_connector_direction;


	function get_direction (
		connector	: in type_net_connector)
		return string;



	
	
	procedure modify_status (
		connector 	: in out type_net_connector;
		operation	: in type_status_operation);



	-- This type models the connectors of
	-- a net segment:
	type type_net_connectors is record
		A : type_net_connector; --(active => false);
		B : type_net_connector; -- (active => false);
	end record;



	

	procedure reset_status (
		labels : in out type_net_connectors);
	

	function is_active (
		connector : in type_net_connector)
		return boolean;


	procedure set_active (
		connector : in out type_net_connector);
	


	function is_proposed (
		connector : in type_net_connector)
		return boolean;

	
							 
	procedure set_proposed (
		connector : in out type_net_connector);


	procedure clear_proposed (
		connector : in out type_net_connector);


	

	function is_selected (
		connector : in type_net_connector)
		return boolean;

	
	procedure set_selected (
		connector : in out type_net_connector);


	procedure clear_selected (
		connector : in out type_net_connector);





	function is_moving (
		connector : in type_net_connector)
		return boolean;

	
	procedure set_moving (
		connector : in out type_net_connector);


	procedure clear_moving (
		connector : in out type_net_connector);

	
	
	
	procedure reset_connector (
		connector : in out type_net_connector);


	

	use et_fonts;
	
	-- GUI relevant only: The font of a net connector:
	net_connector_font : constant type_font := (
		family	=> to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	

	use pac_geometry_sch;
	
	-- GUI relevant only: The line width of the box that enshroudes 
	-- the net name of a connector:
	net_connector_box_linewidth : constant type_distance_positive := 0.2;

	
	-- GUI relevant only: The spacing between anchor point 
	-- of the connector and the net name:
	net_connector_text_offset : constant type_distance_positive := 1.0;

	
	-- GUI relevant only: The ratio of box height 
	-- to text size of a net connector:
	net_connector_height_to_size_ratio : constant type_distance_positive := 1.8;


	
end et_net_connectors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
