using DDDNetCore.Domain.ConnectionRequests;
using DDDNetCore.Domain.ConnectionRequests.DTOS;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class ConnectionRequestsController : ControllerBase
    {
        private readonly ConnectionRequestService _service;

        public ConnectionRequestsController(ConnectionRequestService service)
        {
            _service = service;
        }

        // GET: api/ConnectionRequests
        [HttpGet]
        public async Task<ActionResult<IEnumerable<ConnectionRequestDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Connections/5
        [HttpGet("{id}")]
        public async Task<ActionResult<ConnectionRequestDto>> GetGetById(Guid id)
        {
            var con = await _service.GetByIdAsync(new ConnectionRequestId(id));

            if (con == null)
            {
                return NotFound();
            }

            return con;
        }

        // POST: api/ConnectionRequests/dir
        [HttpPost("dir")]
        public async Task<ActionResult<DirectRequestDto>> CreateDir(CreatingDirectRequestDto dto)
        {
            try
            {
                var con = await _service.AddDirAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = con.Id }, con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // POST: api/ConnectionRequests/intr
        [HttpPost("intr")]
        public async Task<ActionResult<IntroductionRequestDto>> CreateIntr(CreatingIntroductionRequestDto dto)
        {
            try
            {
                var con = await _service.AddIntAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = con.Id }, con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/ConnectionRequests/dir/5
        [HttpPut("dir/{id}")]
        public async Task<ActionResult<DirectRequestDto>> UpdateDir(string id, DirectRequestDto dto)
        {
            if (!id.Equals(dto.Id))
            {
                return BadRequest();
            }

            try
            {
                var con = await _service.UpdateDirAsync(dto);

                if (con == null)
                {
                    return NotFound();
                }
                return Ok(con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/ConnectionRequests/int/5
        [HttpPut("int/{id}")]
        public async Task<ActionResult<IntroductionRequestDto>> UpdateInt(string id, IntroductionRequestDto dto)
        {
            if (!id.Equals(dto.Id))
            {
                return BadRequest();
            }

            try
            {
                var con = await _service.UpdateIntAsync(dto);

                if (con == null)
                {
                    return NotFound();
                }
                return Ok(con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // Inactivate: api/ConnectionRequests/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<ConnectionRequestDto>> SoftDelete(Guid id)
        {
            var con = await _service.InactivateAsync(new ConnectionRequestId(id));

            if (con == null)
            {
                return NotFound();
            }

            return Ok(con);
        }

        // DELETE: api/ConnectionRequests/5/hard
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<ConnectionRequestDto>> HardDelete(Guid id)
        {
            try
            {
                var con = await _service.DeleteAsync(new ConnectionRequestId(id));

                if (con == null)
                {
                    return NotFound();
                }

                return Ok(con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // CRUD OVER //

        // GET: api/ConnectionRequests/pendingRequests/email
        [HttpGet("pendingRequests/{email}")]
        public async Task<ActionResult<IEnumerable<ConnectionRequestDto>>> GetAllUserPendingDirectRequests(string email)
        {
            return await _service.GetAllUserPendingDirectRequestsAsync(email);
        }

        // GET: api/connectionRequests/pendingRequests/emails?emailPlayer=email1@gmail.com&emailTarget=email2@gmail.com
        [HttpGet("pendingRequests/emails")]
        public async Task<ActionResult<ConnectionRequestDto>> GetByEmails(string emailPlayer, string emailTarget)
        {
            try
            {
                var con = await _service.GetByEmailsAsync(emailPlayer, emailTarget);
                if (con == null)
                {
                    return NotFound();
                }
                return con;
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/ConnectionRequests/pendingRequests/email@gmail.com/accept/
        [HttpPut("pendingRequests/{email}/accept")]
        public async Task<ActionResult<AcceptRequestDto>> AcceptRequest(string email, AcceptRequestDto dto)
        {
            if (!email.Equals(dto.Target))
            {
                return BadRequest();
            }

            try
            {
                var conR = await _service.AcceptRequest(dto);

                if (conR == null)
                {
                    return NotFound();
                }

                return Ok(conR);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
    }
}
