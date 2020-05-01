------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        LAYER DISPLAY SCHEMATIC                           --
--                                                                          --
--                               B o d y                                    --
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

-- with ada.text_io;
-- with ada.strings;
-- with ada.strings.bounded;

package body et_display.schematic is

	function grid_enabled return boolean is begin
		if layers.grid = ON then
			return true;
		else
			return false;
		end if;
	end grid_enabled;
	
	function device_names_enabled return boolean is begin
		if layers.device_names = ON then
			return true;
		else
			return false;
		end if;
	end device_names_enabled;	

	function device_values_enabled return boolean is begin
		if layers.device_values = ON then
			return true;
		else
			return false;
		end if;
	end device_values_enabled;	

	function device_purposes_enabled return boolean is begin
		if layers.device_purposes = ON then
			return true;
		else
			return false;
		end if;
	end device_purposes_enabled;	
	
	function ports_enabled return boolean is begin
		if layers.ports = ON then
			return true;
		else
			return false;
		end if;
	end ports_enabled;
	
	function nets_enabled return boolean is begin
		if layers.nets = ON then
			return true;
		else
			return false;
		end if;
	end nets_enabled;

	function texts_enabled return boolean is begin
		if layers.texts= ON then
			return true;
		else
			return false;
		end if;
	end texts_enabled;

	
end et_display.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
