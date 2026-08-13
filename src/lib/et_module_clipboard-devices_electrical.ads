------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE CLIPBOARD DEVICES ELECTRICAL                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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
--  To Do:
--
--


with et_module;						use et_module;
with et_generic_modules;			use et_generic_modules;
with et_schematic_coordinates;		use et_schematic_coordinates;
with et_devices_electrical;			use et_devices_electrical;
with et_units;						use et_units;
with et_logging;					use et_logging;



package et_module_clipboard.devices_electrical is


	-- This procedure copies a given device and its unit
	-- to the clipboard.
	-- 1. If the device does not exist in the clipboard yet,
	--    then a new device is created there.
	-- 2. If the device does exist, then it will not be created
	--    anew.
	-- 3. The given unit is copied into the device.
	procedure copy_unit_to_clipboard (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_cursor		: in pac_units.cursor;
		log_threshold	: in type_log_level);



	-- This procedure pastes a device and its unit
	-- from the clipboard into the given target module.
	-- Arguments device_cursor and unit_cursor indicate
	-- the device and unit in the clipboard.
	procedure paste_unit_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_electrical.cursor;
		unit_cursor		: in pac_units.cursor;
		offset			: in type_object_position_relative;
		-- target_device	: in type_device_name := device_name_default;
		-- device_created	: out type_device_name;
		log_threshold	: in type_log_level);

	

end et_module_clipboard.devices_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
