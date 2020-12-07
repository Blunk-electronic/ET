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
with ada.characters.handling;	use ada.characters.handling;

package body et_assembly_variants is

	function to_mounted (mounted : in string) return type_mounted is begin
		return type_mounted'value (mounted);
	end;
		
	function to_mounted (mounted : in type_mounted) return string is begin
		return space & to_lower (type_mounted'image (mounted));
	end;

	function is_mounted (
		device	: in type_device_name; -- IC1
		variant	: in pac_variants.cursor)
		return boolean is
	-- Returns true if the given device is to be mounted according to given assembly variant.
	-- If variant points to no element the default variant is assumed and the device regarded as mounted.
		
		use pac_variants;
		use type_devices;
		
		cursor : type_devices.cursor;
		
		procedure query_devices (
			variant_name	: in et_general.pac_assembly_variant_name.bounded_string; -- low_cost
			variant			: in type_assembly_variant) is
		begin
			cursor := find (variant.devices, device);
		end query_devices;
		
	begin -- is_mounted
		if variant = pac_variants.no_element then -- assume default variant
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
	
end et_assembly_variants;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
