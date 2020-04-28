------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            LAYER DISPLAY                                 --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
--   ToDo: 

package et_display is

	type type_layer_status is (ON, OFF);

	default : constant type_layer_status := OFF;

	function to_string (on_off : in type_layer_status) return string;
	function to_layer_status (on_off : in string) return type_layer_status;

-- SCHEMATIC
	type type_schematic_layers is record
		nets			: type_layer_status := ON;
		ports			: type_layer_status := OFF; -- the circles around the start point of ports
		-- ?? net_labels		: type_layer_status := ON;
		device_names	: type_layer_status := ON; -- for things like IC12, submodule names, instances and position
		device_values	: type_layer_status := ON; -- things like 220R or 7400
		device_purposes	: type_layer_status := ON; -- things like "brightness control"
		texts			: type_layer_status := ON; -- general notes
	end record;

	-- This global variable is read whenever things are displayed in a schematic:
	schematic_layers : type_schematic_layers;

end et_display;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
