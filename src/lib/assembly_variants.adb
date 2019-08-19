------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          ASSEMBLY VARIANTS                               --
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

--   The two letters "CS" indicate a "construction site" where things are not
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

	function is_mounted (
		device	: in et_libraries.type_device_name; -- IC1
		variant	: in type_variants.cursor)
		return boolean is
	-- Returns true if the given device is to be mounted according to given assembly variant.
	-- If variant points to no element the default variant is assumed and the device regarded as mounted.
		
		use type_variants;
		use type_devices;
		
		cursor : type_devices.cursor;
		
		procedure query_devices (
			variant_name	: in type_variant_name.bounded_string; -- low_cost
			variant			: in type_variant) is
		begin
			cursor := find (variant.devices, device);
		end query_devices;
		
	begin -- is_mounted
		if variant = type_variants.no_element then -- assume default variant
			return true; -- device is to be mounted
		else
			
			query_element (
				position	=> variant,
				process		=> query_devices'access);

			if cursor = type_devices.no_element then
				-- Device has no entry in assembly variant and thus is to be mounted
				-- as it is in the default variant:
				return true;
			else
				-- Device has an entry in assembly variant. The question now
				-- is whether the entry requests the device mounted or not.
				if element (cursor).mounted = YES then
					return true;
				else
					return false;
				end if;
				
			end if;

		end if;
		
-- 		exception
-- 			when event: others =>
-- 				log_indentation_reset;
-- 				log (text => "B " & ada.exceptions.exception_information (event), console => true);
-- 				raise;
		
	end is_mounted;
	
end assembly_variants;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
