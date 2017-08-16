------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET LIBRARIES                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
--with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_schematic;				use et_schematic;

with et_geometry;				use et_geometry;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;

package body et_libraries is

	function to_string ( name_in_library : in type_component_name.bounded_string) return string is
	-- Returns the given name_in_library as as string.
	-- CS: provide a parameter that turns the pretext like "name in library" on/off
	begin
		return ("name in library " & type_component_name.to_string(name_in_library));
	end to_string;

	function to_string ( packge : in type_component_package_name.bounded_string) return string is
	-- Returns the given package name as as string.
	-- CS: provide a parameter that turns the pretext on/off
	begin
		return (type_component_package_name.to_string(packge));
	end to_string;
	
	function to_string ( variant : in type_component_variant) return string is
	-- Returns the given variant as string.
	-- NOTE: This displays the type_component_variant (see et_libraries.ads).
	-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
	-- like in TL084D or TL084N.
	-- CS: provide a parameter that turns the pretext on/off ? Useful ?

	-- If the library name or package name of given variant is empty, assume item_not_specified.
	-- This produces a still readable output like "library item_not_specified package item_not_specified".
		v : type_component_variant := variant;
	begin
		if type_library_full_name.length(v.library) = 0 then
			v.library := type_library_full_name.to_bounded_string(et_general.item_not_specified);
		end if;

		if type_component_package_name.length(v.packge) = 0 then
			v.packge := type_component_package_name.to_bounded_string(et_general.item_not_specified);
		end if;
		
		return ("library " & type_library_full_name.to_string(v.library)
			& " package " & type_component_package_name.to_string(v.packge));
	end to_string;


	function text_meaning_to_string ( meaning : in type_text_meaning) return string is
	-- Converts meaning to string.
		function strip_prefix ( s : in string) return string is
		begin
			return s (s'first + text_meaning_prefix'last .. s'last);
		end strip_prefix;
	begin
		-- Since the text meaning may assume p_function 
		-- (which is a workaround in order not to use an ada keyword. see spec of this package),
		-- we must get gid of the prefix:
		case meaning is
			when p_function => -- we must remove the prefix
				return strip_prefix(to_lower(type_text_meaning'image(meaning)));
			when others => -- we can do a direct conversion
				return to_lower(type_text_meaning'image(meaning));
		end case;
	end text_meaning_to_string;

	
	procedure write_placeholder_properties ( placeholder : in type_text_placeholder; indentation : in natural := 0) is
	-- Writes the properties of the given placeholder.
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		-- meaning
		put_line(indent(indentation) 
			& et_libraries.text_meaning_to_string(placeholder.meaning));

		-- position
-- 		put_line(indent(indentation + 1)
-- 			 & et_libraries.to_string (placeholder.position));

		-- CS:
		
		-- size

		-- style

		-- line width

		-- orientation

		-- visible

		-- alignment

		
-- 			& trim(et_general.type_grid'image(text.position.x),left) & "/"
-- 			& trim(et_general.type_grid'image(text.position.y),left));
-- 		put_line(indentation * latin_1.space & "size (mm/mil) " 
-- 			& "?/" -- CS
-- 			& trim(et_libraries.type_text_size'image(text.size),left));
-- 		put_line(indentation * latin_1.space & "style " & to_lower(et_libraries.type_text_style'image(text.style)));
-- 		put_line(indentation * latin_1.space & "line width" & et_libraries.type_text_line_width'image(text.line_width));
-- 		put_line(indentation * latin_1.space & "orientation " & et_general.type_orientation'image(text.orientation));
-- 		put_line(indentation * latin_1.space & "aligment (hor/vert) " 
-- 			& to_lower(et_libraries.type_text_alignment_horizontal'image(text.alignment.horizontal))
-- 			& "/"
-- 			& to_lower(et_libraries.type_text_alignment_vertical'image(text.alignment.vertical)));
-- 		put_line(indentation * latin_1.space & "visible " & et_libraries.type_text_visible'image(text.visible));

		
	end write_placeholder_properties;
	
end et_libraries;

-- Soli Deo Gloria
