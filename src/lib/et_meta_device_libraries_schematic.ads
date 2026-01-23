------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                  META / DEVICE LIBRARIES / SCHEMATIC                     --
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
--   ToDo: 


with ada.strings.bounded;       use ada.strings.bounded;

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;


package et_meta_device_libraries_schematic is


	-- PATHS FOR PREFERRED LIBRARIES:

	-- NOTE: System ET does not follow the classical approach of
	-- search paths for libraries. A library here is just a directory where
	-- device models (*.dev) or non-electrical packages (*.pac) live.
	-- Device models and non-electrical packages can be everywhere.
	-- In graphical mode, when adding a device in the schematic or a non-electrical package in
	-- the board/layout there is a window where the operator selects
	-- a model. This window just proposes the paths of preferred libraries.
	-- The operator is not restricted to those library paths and is
	-- free to store models wherever it suits her/him.
	
	-- A preferred directory that contains devices (*.dev)
	-- like "$HOME/git/BEL/ET_component_library/devices":
	prf_lib_sch_length_max : constant positive := 100;
	
	package pac_preferred_library_schematic is new 
		generic_bounded_length (prf_lib_sch_length_max);
	
	use pac_preferred_library_schematic;


	-- Returns true if the given path exists:
	function library_path_exists (
		lib : in pac_preferred_library_schematic.bounded_string)
		return boolean;
		
	
	function to_preferred_library_schematic (lib : in string)
		return pac_preferred_library_schematic.bounded_string;

	function to_string (lib : in pac_preferred_library_schematic.bounded_string)
		return string;
	
	package pac_preferred_libraries_schematic is new 
		doubly_linked_lists (pac_preferred_library_schematic.bounded_string);
	-- CS rename to pac_preferred_libraries_devices ?
		


	
end et_meta_device_libraries_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
