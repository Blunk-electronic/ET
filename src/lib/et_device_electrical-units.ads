------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICE ELECTRICAL / UNITS                           --
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
-- The subprograms in this package address the units
-- of an electrical device. So the targeted device must be real.
-- Otherwise exceptions wlll be raised.
-- 1. Here it is about coordinates, location vectors
--    places, angles and rotation as they are used in
--    the schematic domain exclusively.


with et_coordinates_formatting;			use et_coordinates_formatting;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;

with et_sheets;							use et_sheets;
with et_schematic_geometry;				use et_schematic_geometry;
-- with et_device_appearance;				use et_device_appearance;

-- with et_device_placeholders;			use et_device_placeholders;
with et_units;							use et_units;



package et_device_electrical.units is

	use pac_geometry_2;


-- POSITION:



	function get_unit_positions (
		device	: in type_device_electrical)
		return pac_unit_positions.map;


	procedure move_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name;
		coordinates	: in type_coordinates;
		destination	: in type_vector_model;
		sheet		: in type_sheet_relative);


	procedure drag_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name;
		coordinates	: in type_coordinates;
		destination	: in type_vector_model;
		sheet		: in type_sheet_relative);


	procedure rotate_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name;
		coordinates	: in type_coordinates;
		rotation	: in type_rotation_model);


	-- Toggles the mirror status:
	procedure mirror_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name);



	-- Moves all units which are on the given sheet
	-- by the given offset:
	procedure move_units_by_sheets (
		device		: in out type_device_electrical;
		sheet		: in type_sheet;
		offset		: in type_sheet_relative);




	-- Copies a unit into the given device. The copy
	-- will be placed by the given number of sheets and
	-- the given offset (x/y) away from the original unit.
	-- CS:
	-- 1. Currently the caller must ensure that the unit
	--    can become a part of the given device as specified.
	--    The source device and the target device must have the
	--    same device model.
	-- 2. If the unit already exists in the given device,
	--    then an exception is raised.
	procedure copy_unit_to_device (
		source_unit_cursor	: in pac_units.cursor;
		sheet				: in type_sheet_relative;
		offset				: in type_vector_model;
		target_device		: in out type_device_electrical);


	-- Sets all units or an explicitly given unit as selected.
	-- If all_units is true, then the parameter "unit" is ignored:
	procedure select_unit (
		device		: in out type_device_electrical;
		all_units	: in boolean;
		unit_name	: in type_unit_name);


	-- Modifies the status of a unit:
	procedure modify_status (
		device		: in out type_device_electrical;
		unit_cursor	: in pac_units.cursor;
		operation	: in type_status_operation);


	-- Calls a procedure that modifies the given
	-- unit:
	procedure modify_unit (
		device		: in out type_device_electrical;
		unit_cursor	: in pac_units.cursor;
		process		: not null access procedure (
				name	: in type_unit_name;
				unit	: in out type_unit));


end et_device_electrical.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
