program raytracing
    use vec3_module
    use ray_module
    use sphere_module
    use ppm

    implicit none

    type(vec3) :: white, background, main_color 
    type(vec3) :: pi, L, N, pix_col

    type(sphere) :: sph, light
    type(sphere), dimension(3) :: objects

    type(ray) :: ry

    real :: t, dt

    integer :: H, W, x, y, i

    logical :: intersects

    white = vec3(255.0, 255.0, 255.0)
    background = vec3(30.0, 30.0, 30.0)

    ! sph = sphere(vec3(250.0, 250.0, 100.0), 100.0)
    objects = (/sphere(vec3(200.0, 350.0, 100.0), 100.0, vec3(190.0, 239.0, 85.0)), &
                sphere(vec3(500.0, 350.0, 100.0), 100.0, vec3(186.0, 59.0, 70.0)), & 
                sphere(vec3(20.0, 20.0, 100.0), 10.0, white) /) ! Light source indicator

    light = sphere(vec3(0.0, 0.0, 100.0), 1.0, white)

    t = 0.0

    H = 700
    W = 700

    call ppm_init(W, H)

    do y=1, H
        do x=1, W
            pix_col = background

            ry = ray(vec3(real(x), real(y), 0.0), vec3(0.0, 0.0, 1.0))

            do i = 1, size(objects)
                sph = objects(i)

                call intersect(sph, ry, t, intersects)

                if (intersects) then
                    pi = ry%o + ry%d * t
                    L = light%c - pi
                    N = get_normal(sph, pi)
                    dt = dot(normalize(L), normalize(N))

                    pix_col = (sph%color + white*dt) * 0.5
                    pix_col = clamp(255.0, pix_col)
                end if
            end do

            call point(x, y, color(int(pix_col%x), int(pix_col%y), int(pix_col%z)))
        end do
    end do

    call render_image(1, "o", .true.)

    call ppm_release()

end program raytracing