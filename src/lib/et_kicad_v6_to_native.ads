------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     KICAD V6 TO NATIVE CONVERSION                        --
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

-- DESCRIPTION:
--
--   Converts an already-loaded KiCad v6 schematic project (see
--   et_kicad_v6.schematic.import_design) into ET's native in-memory data
--   structures, and optionally saves the result as a native ET project.
--
--   Mirrors et_kicad_to_native's shape: device models are synthesized from
--   the KiCad symbol library data embedded in the project (there is no
--   pre-existing ET device library to link against -- see
--   et_device_library.get_device_model's contract, which requires the
--   model to already be in the rig-wide device_library) and inserted into
--   that same rig-wide device_library, exactly like et_kicad_to_native's
--   copy_components/query_components does for v4/v5 projects.
--
--   The whole sheet hierarchy is flattened into ONE native module, again
--   mirroring et_kicad_to_native (its "transpose" step does the same for
--   v4/v5). PCB/board conversion is out of scope -- this project has no
--   footprint or layout data to convert.
--
--   history of changes:
--

with et_logging;					use et_logging;
with et_module;						use et_module;
with et_kicad_v6.schematic;

package et_kicad_v6_to_native is

	-- Synthesizes ET device models (into the rig-wide device_library) and
	-- one flattened native module from the given, already-loaded KiCad v6
	-- project. Does not touch the file system. This is the entry point
	-- used by the testbench to verify the conversion in memory:
	function convert (
		project			: in et_kicad_v6.schematic.type_project;
		log_threshold	: in type_log_level)
		return type_generic_module;

	-- Calls convert, then creates a native ET project directory named
	-- after the project, and saves the resulting module plus every
	-- synthesized device model into it -- mirrors et_kicad_to_native.
	-- to_native's shape (project directory + *.mod + libraries/devices/
	-- *.dev).
	procedure to_native (
		project			: in et_kicad_v6.schematic.type_project;
		log_threshold	: in type_log_level);

end et_kicad_v6_to_native;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
