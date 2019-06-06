------------------------------------------------------------------------------
--                                                                          --
--                       SYSTEM ET ASSEMBLY VARIANTS                        --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.unbounded;
with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;
-- with gnat.directory_operations;

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_general;				use et_general;
with et_coordinates;
with et_string_processing;		use et_string_processing;
with et_libraries;				use et_libraries;
with et_schematic;				use et_schematic;
with et_pcb;
with et_pcb_coordinates;
with et_project;				use et_project;
with submodules;
with conventions;
with et_geometry;

package body assembly_variants is

	function to_mounted (mounted : in string) return type_mounted is begin
		return type_mounted'value (mounted);
	end;
		
	function to_mounted (mounted : in type_mounted) return string is begin
		return latin_1.space & to_lower (type_mounted'image (mounted));
	end;

	function to_variant (variant : in type_variant_name.bounded_string) return string is begin
		return type_variant_name.to_string (variant);
	end;

	function to_variant (variant : in string) return type_variant_name.bounded_string is begin
		return type_variant_name.to_bounded_string (variant);
	end;
		
end assembly_variants;
	
-- Soli Deo Gloria
