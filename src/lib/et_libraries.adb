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

	function to_string ( position : in type_coordinates) return string is
	-- Returns the given position as string.
	begin
		return coordinates_preamble
			& trim (et_libraries.type_grid'image(position.x),left) 
			& coordinates_dimension_separator
			& trim (et_libraries.type_grid'image(position.y),left);

	end to_string;

	function to_string (orientation : in type_orientation) return string is
	-- Returns the the given orientation as string.
		preamble	: constant string (1..20) := "orientation/rotation";
		suffix		: constant string (1..4)  := " deg";
	begin
		return (preamble & type_orientation'image(orientation) & suffix);
	end to_string;
	
	function to_string ( name_in_library : in type_component_name.bounded_string) return string is
	-- Returns the given name_in_library as as string.
	-- CS: provide a parameter that turns the pretext like "name in library" on/off
	begin
		--return ("name in library " & type_component_name.to_string(name_in_library));
		return (type_component_name.to_string(name_in_library));
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


	function to_string ( meaning : in type_text_meaning) return string is
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
	end to_string;

	
	procedure write_placeholder_properties ( placeholder : in type_text_placeholder; indentation : in natural := 0) is
	-- Writes the properties of the given placeholder.
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		-- meaning
		put_line(indent(indentation) 
			& et_libraries.to_string(placeholder.meaning));

		-- position
		put_line(indent(indentation + 1)
			& et_libraries.to_string (placeholder.position));

		-- size
		put_line(indent(indentation + 1)
			& "size"
			& et_libraries.type_text_size'image (placeholder.size));

		-- style
		put_line(indent(indentation + 1)
			& "style "
			& to_lower(et_libraries.type_text_style'image (placeholder.style)));

		-- line width
		put_line(indent(indentation + 1)
			& "line width"
			& et_libraries.type_text_line_width'image (placeholder.line_width));

		-- orientation
		put_line(indent(indentation + 1)
			& et_libraries.to_string (placeholder.orientation)); 

		-- visible
		put_line(indent(indentation + 1)
			& "visible "
			& to_lower(et_libraries.type_text_visible'image (placeholder.visible)));

		-- alignment
		put_line(indent(indentation + 1)
			& "aligment (hor/vert) "
			& to_lower(et_libraries.type_text_alignment_horizontal'image(placeholder.alignment.horizontal))
			& "/"
			& to_lower(et_libraries.type_text_alignment_vertical'image(placeholder.alignment.vertical)));
	end write_placeholder_properties;


	procedure write_text_properies ( text : in et_libraries.type_text; indentation : in natural := 0) is
	-- Outputs the properties of the given text.
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		-- meaning
		put_line(indent(indentation) 
			--& "meaning "
			& et_libraries.to_string(text.meaning));

		-- position
		put_line(indent(indentation + 1)
			& et_libraries.to_string (text.position));

		-- content
		if et_libraries.type_text_content.length(text.content) > 0 then
			put_line(indent(indentation + 1) 
				& "content '" & et_libraries.type_text_content.to_string(text.content) & "'");
		else
			put_line(indent(indentation + 1)
				& "no content");
		end if;
		
		-- size
		put_line(indent(indentation + 1)
			& "size"
			& et_libraries.type_text_size'image (text.size));

		-- style
		put_line(indent(indentation + 1)
			& "style "
			& to_lower(et_libraries.type_text_style'image (text.style)));

		-- line width
		put_line(indent(indentation + 1)
			& "line width"
			& et_libraries.type_text_line_width'image (text.line_width));

		-- orientation
		put_line(indent(indentation + 1)
			& et_libraries.to_string (text.orientation));

		-- visible
		put_line(indent(indentation + 1)
			& "visible "
			& to_lower(et_libraries.type_text_visible'image (text.visible)));

		-- alignment
		put_line(indent(indentation + 1)
			& "aligment (hor/vert) "
			& to_lower(et_libraries.type_text_alignment_horizontal'image(text.alignment.horizontal))
			& "/"
			& to_lower(et_libraries.type_text_alignment_vertical'image(text.alignment.vertical)));

	end write_text_properies;


	function content ( text : in type_text) return string is
	-- Returns the content of the given text as string.
		c : type_text_content.bounded_string;
	begin
		c := text.content;
		return type_text_content.to_string(c);
	end content;
	
end et_libraries;

-- Soli Deo Gloria
