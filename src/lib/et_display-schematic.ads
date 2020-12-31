------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      LAYER DISPLAY SCHEMATIC                             --
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

package et_display.schematic is

	type type_layers is record
		grid			: type_layer_status := OFF;
		nets			: type_layer_status := ON;
		ports			: type_layer_status := OFF; -- the circles around the start point of ports
		-- ?? net_labels		: type_layer_status := ON;
		-- ?? device_origins -- when exporting images disable automatically ?
		device_names	: type_layer_status := ON; -- for things like IC12, submodule names, instances and position
		device_values	: type_layer_status := ON; -- things like 220R or 7400
		device_purposes	: type_layer_status := ON; -- things like "brightness control"
		texts			: type_layer_status := ON; -- general notes
	end record;

	-- This global variable is read whenever things are displayed in a schematic:
	layers : type_layers;

	-- Returns true if grid layer is enabled:
	function grid_enabled return boolean;
	
	-- Returns true if device names layer is enabled:
	function device_names_enabled return boolean;

	-- Returns true if device values layer is enabled:
	function device_values_enabled return boolean;

	-- Returns true if device purposes layer is enabled:
	function device_purposes_enabled return boolean;
	
	-- Returns true if port layer is enabled:
	function ports_enabled return boolean;
	
	-- Returns true if net layer is enabled:
	function nets_enabled return boolean;

	-- Returns true if text layer is enabled:
	function texts_enabled return boolean;

	
end et_display.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
