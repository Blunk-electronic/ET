------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICE ELECTRICAL / PACKAGE                         --
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
-- 1. device accessories
-- 2. see comments on package pac_devices_electrical
--
--
-- DESCRIPTION:
--
-- This package is about the type, basic properties and subprograms related
-- to a called "electrical device" as it is modeled in the schematic
-- and in the board.
-- These devices have a representation in schematic and mostly in the board.


with et_board_coordinates;
with et_device_unit;					use et_device_unit;
with et_device_prefix;					use et_device_prefix;
with et_device_appearance;				use et_device_appearance;
with et_device_purpose;					use et_device_purpose;
with et_device_model_names;				use et_device_model_names;
with et_device_value;					use et_device_value;
with et_device_name;					use et_device_name;
with et_device_partcode;				use et_device_partcode;
with et_device_properties;				use et_device_properties;
with et_device_library;					use et_device_library;
with et_device_placeholders.packages;
with et_package_model_name;				use et_package_model_name;
with et_package_variant_name;			use et_package_variant_name;
with et_object_status;					use et_object_status;
with et_units;							use et_units;
with et_logging;						use et_logging;


package et_device_electrical.packge is


	procedure dummy;


end et_device_electrical.packge;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
