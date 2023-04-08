module hit_record_module
    use vec3_module
    use ray_module
    use material_module
    implicit none

    type hit_record
        type(vec3) :: p, normal
        real :: t
        logical :: front_face
        type(material) :: mat
    end type hit_record

contains

    subroutine set_face_normal(hit_r, r, outward_normal)
        type(hit_record), intent(out) :: hit_r
        type(ray), intent(in) :: r
        type(vec3), intent(in) :: outward_normal 

        hit_r%front_face = dot(r%direction, outward_normal) .lt. 0
        
        if (hit_r%front_face) then
            hit_r%normal = outward_normal
        else
            hit_r%normal = outward_normal * (-1.0)
        end if

    end subroutine set_face_normal

end module hit_record_module