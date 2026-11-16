------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICE PROPERTIES                                --
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
-- To Do:
--
--
--
--
-- DESCRIPTION:
--
--
--
--
--


with et_device_purpose;					use et_device_purpose;
with et_device_value;					use et_device_value;
with et_device_partcode;				use et_device_partcode;


package et_device_properties is


	type type_device_properties is private;



	procedure set_value (
		properties	: in out type_device_properties;
		value		: in type_device_value);


	function get_value (
		properties	: in type_device_properties)
		return type_device_value;



	procedure set_partcode (
		properties	: in out type_device_properties;
		partcode	: in type_device_partcode);


	function get_partcode (
		properties	: in type_device_properties)
		return type_device_partcode;



	procedure set_purpose (
		properties	: in out type_device_properties;
		purpose		: in type_device_purpose);


	function get_purpose (
		properties	: in type_device_properties)
		return type_device_purpose;


private


	type type_device_properties is record
		value		: type_device_value; -- 470R

		partcode	: type_device_partcode; -- R_PAC_S_0805_VAL_100R
		-- For virtual packages (test points, edge connectors, ...)
		-- usually no partcode is required.

		-- The purpose indicates what the device is doing.
		-- It is usually required for devices that require interaction
		-- with the user of a PCBA:
		purpose		: type_device_purpose; -- brightness_control
	end record;
	

end et_device_properties;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
